(** Lwt support for Curl *)

module M = Curl.Multi

module Pool = struct
  type state = Open | Closing | Closed

  type t = {
    mt : M.mt;
    all_events : (Unix.file_descr, Lwt_engine.event list) Hashtbl.t;
    wakeners : (Curl.t, Curl.curlCode Lwt.u) Hashtbl.t;
    mutable timer_event : Lwt_engine.event;
    mutable state : state;
  }

  let ensure_open name t =
    match t.state with
    | Open -> ()
    | Closing | Closed -> invalid_arg ("Curl_lwt.Pool." ^ name ^ ": pool is closed")

  let stop_timer t =
    Lwt_engine.stop_event t.timer_event;
    t.timer_event <- Lwt_engine.fake_event

  let stop_fd_events t fd =
    match Hashtbl.find_opt t.all_events fd with
    | None -> ()
    | Some events ->
      List.iter Lwt_engine.stop_event events;
      Hashtbl.remove t.all_events fd

  let stop_all_events t =
    Hashtbl.iter
      (fun _ events -> List.iter Lwt_engine.stop_event events)
      t.all_events;
    Hashtbl.clear t.all_events

  let finished t =
    let rec collect completed =
      match M.remove_finished t.mt with
      | None -> completed
      | Some (h, code) ->
        begin match Hashtbl.find_opt t.wakeners h with
        | None ->
          prerr_endline "curl_lwt: orphan handle, how come?";
          collect completed
        | Some wakener ->
          Hashtbl.remove t.wakeners h;
          collect ((wakener, code) :: completed)
        end
    in
    List.iter
      (fun (wakener, code) -> Lwt.wakeup wakener code)
      (List.rev (collect []))

  let create () =
    let t = {
      mt = M.create ();
      all_events = Hashtbl.create 32;
      wakeners = Hashtbl.create 32;
      timer_event = Lwt_engine.fake_event;
      state = Open;
    } in
    let on_readable fd _ =
      match t.state with
      | Closing | Closed -> ()
      | Open ->
        let (_ : int) = M.action t.mt fd M.EV_IN in
        finished t
    in
    let on_writable fd _ =
      match t.state with
      | Closing | Closed -> ()
      | Open ->
        let (_ : int) = M.action t.mt fd M.EV_OUT in
        finished t
    in
    let on_timer _ =
      match t.state with
      | Closing | Closed -> ()
      | Open ->
        stop_timer t;
        let (_ : int) = M.action_timeout' t.mt in
        finished t
    in
    M.set_timer_function t.mt begin fun timeout ->
      stop_timer t;
      if t.state = Open && timeout >= 0 then
        t.timer_event <-
          Lwt_engine.on_timer
            (float_of_int timeout /. 1000.) false on_timer
    end;
    M.set_socket_function t.mt begin fun fd what ->
      stop_fd_events t fd;
      if t.state = Open then begin
        let events = match what with
        | M.POLL_REMOVE | M.POLL_NONE -> []
        | M.POLL_IN -> [Lwt_engine.on_readable fd (on_readable fd)]
        | M.POLL_OUT -> [Lwt_engine.on_writable fd (on_writable fd)]
        | M.POLL_INOUT ->
          [ Lwt_engine.on_readable fd (on_readable fd);
            Lwt_engine.on_writable fd (on_writable fd) ]
        in
        match events with
        | [] -> ()
        | _ -> Hashtbl.add t.all_events fd events
      end
    end;
    t

  let setopt t opt =
    ensure_open "setopt" t;
    M.setopt t.mt opt

  let perform t h =
    ensure_open "perform" t;
    if Hashtbl.mem t.wakeners h then
      invalid_arg "Curl_lwt.Pool.perform: Curl handle is already active";
    let waiter, wakener = Lwt.wait () in
    let waiter = Lwt.protected waiter in
    Lwt.on_cancel waiter begin fun () ->
      if t.state = Open && Hashtbl.mem t.wakeners h then begin
        Hashtbl.remove t.wakeners h;
        M.remove t.mt h
      end
    end;
    Hashtbl.add t.wakeners h wakener;
    begin match M.add t.mt h with
    | () -> waiter
    | exception exn ->
      Hashtbl.remove t.wakeners h;
      raise exn
    end

  let abort t h =
    match t.state, Hashtbl.find_opt t.wakeners h with
    | Open, Some wakener ->
      (* Removing a handle does not enqueue a completion message. Resolve the
         waiter ourselves, normally rather than through Lwt cancellation. *)
      M.remove t.mt h;
      Hashtbl.remove t.wakeners h;
      Lwt.wakeup_later wakener Curl.CURLE_ABORTED_BY_CALLBACK
    | _ -> ()

  let cleanup t =
    match t.state with
    | Closing | Closed -> ()
    | Open ->
      t.state <- Closing;
      stop_timer t;
      stop_all_events t;
      let active = Hashtbl.fold (fun h w acc -> (h, w) :: acc) t.wakeners [] in
      Hashtbl.clear t.wakeners;
      let first_error = ref None in
      let protect f =
        match f () with
        | () -> ()
        | exception exn ->
          if !first_error = None then first_error := Some exn
      in
      List.iter (fun (h, _) -> protect (fun () -> M.remove t.mt h)) active;
      protect (fun () -> M.cleanup t.mt);
      t.state <- Closed;
      List.iter
        (fun (_, wakener) ->
          Lwt.wakeup_later wakener Curl.CURLE_ABORTED_BY_CALLBACK)
        active;
      begin match !first_error with
      | None -> ()
      | Some exn -> raise exn
      end

  let with_pool f =
    let t = create () in
    Lwt.finalize
      (fun () -> f t)
      (fun () ->
        cleanup t;
        Lwt.return_unit)
end

(* Lwt callbacks run in one event loop, so a single default pool is sufficient. *)
let global = lazy (Pool.create ())

let setopt opt =
  Pool.setopt (Lazy.force global) opt

let perform h =
  Pool.perform (Lazy.force global) h
