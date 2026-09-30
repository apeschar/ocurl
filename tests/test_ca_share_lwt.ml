let test () =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let open Lwt.Syntax in
  let* () = Lwt_unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) in
  Lwt_unix.listen socket 4;
  let port = match Lwt_unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port | _ -> assert false in
  let share = Curl.Share.create_ca_cache () in
  let fetch () =
    let pool = Curl_lwt.Pool.create () in
    let easy = Curl.init () in
    Curl.set_share easy (Some share);
    Curl.set_url easy (Printf.sprintf "http://127.0.0.1:%d/" port);
    Curl.set_writefunction easy String.length;
    Lwt.finalize
      (fun () ->
        let* code = Curl_lwt.Pool.perform pool easy in
        assert (code = Curl.CURLE_OK);
        Lwt.return_unit)
      (fun () -> Curl_lwt.Pool.cleanup pool; Curl.cleanup easy; Lwt.return_unit)
  in
  let serve () =
    let* fd, _ = Lwt_unix.accept socket in
    let input = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
    let output = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
    let rec headers () =
      let* line = Lwt_io.read_line input in
      if line = "\r" || line = "" then Lwt.return_unit else headers () in
    let* () = headers () in
    let* () = Lwt_io.write output "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok" in
    Lwt_io.close output
  in
  Lwt.finalize
    (fun () -> Lwt.pick [
      (let* () = Lwt.join [fetch (); fetch (); serve (); serve ()] in Lwt.return_unit);
      (let* () = Lwt_unix.sleep 5. in failwith "shared CA Lwt HTTP requests timed out")])
    (fun () -> Lwt_unix.close socket)

let () = Lwt_main.run (test ())
