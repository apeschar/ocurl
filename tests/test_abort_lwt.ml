(* Explicit abort must finish stalled transfers without cancelling promises or
   disturbing other handles on the same multi handle. All servers are local. *)
let test () =
  let open Lwt.Syntax in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) in
  Lwt_unix.listen socket 4;
  let port = match Lwt_unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port | _ -> assert false in
  let pool = Curl_lwt.Pool.create () in
  let slow = Curl.init () and fast = Curl.init () in
  let url path = Printf.sprintf "http://127.0.0.1:%d/%s" port path in
  List.iter (fun h -> Curl.set_writefunction h String.length) [slow; fast];
  let started, wake_started = Lwt.wait () in
  let release, wake_release = Lwt.wait () in
  let early_started, wake_early_started = Lwt.wait () in
  let early_release, wake_early_release = Lwt.wait () in
  let serve () =
    let* fd, _ = Lwt_unix.accept socket in
    let input = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
    let output = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
    Lwt.finalize
      (fun () ->
        let* request = Lwt_io.read_line input in
        let rec headers () =
          let* line = Lwt_io.read_line input in
          if line = "\r" || line = "" then Lwt.return_unit else headers () in
        let* () = headers () in
        if String.starts_with ~prefix:"GET /early " request then (
          Lwt.wakeup_later wake_early_started ();
          early_release)
        else if String.starts_with ~prefix:"GET /slow " request then (
          let* () = Lwt_io.write output
            "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\n" in
          let* () = Lwt_io.flush output in
          Lwt.wakeup_later wake_started ();
          release)
        else Lwt_io.write output
          "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok")
      (fun () -> Lwt_io.close output)
  in
  Lwt.finalize
    (fun () ->
      Curl.set_url slow (url "slow");
      Curl.set_url fast (url "fast");
      let stalled = Curl_lwt.Pool.perform pool slow in
      let unaffected = Curl_lwt.Pool.perform pool fast in
      let servers = Lwt.join [serve (); serve (); serve (); serve ()] in
      let* () = started in
      Curl_lwt.Pool.abort pool slow;
      Curl_lwt.Pool.abort pool slow;
      let* code = stalled in
      assert (code = Curl.CURLE_ABORTED_BY_CALLBACK);
      let* code = unaffected in
      assert (code = Curl.CURLE_OK);
      Curl_lwt.Pool.abort pool fast;
      Curl.set_url slow (url "fast");
      let* code = Curl_lwt.Pool.perform pool slow in
      assert (code = Curl.CURLE_OK);
      Curl.set_url slow (url "early");
      let early = Curl_lwt.Pool.perform pool slow in
      let* () = early_started in
      Curl_lwt.Pool.abort pool slow;
      let* code = early in
      assert (code = Curl.CURLE_ABORTED_BY_CALLBACK);
      let cleaning = Curl_lwt.Pool.perform pool slow in
      Curl_lwt.Pool.cleanup pool;
      let* code = cleaning in
      assert (code = Curl.CURLE_ABORTED_BY_CALLBACK);
      Curl_lwt.Pool.abort pool slow;
      Curl_lwt.Pool.cleanup pool;
      Lwt.wakeup_later wake_release ();
      Lwt.wakeup_later wake_early_release ();
      servers)
    (fun () ->
      Curl_lwt.Pool.cleanup pool;
      Curl.cleanup slow;
      Curl.cleanup fast;
      Lwt_unix.close socket)

let () = Lwt_main.run (test ())
