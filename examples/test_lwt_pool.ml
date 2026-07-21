let fail message =
  prerr_endline message;
  exit 1

let easy () =
  let h = Curl.init () in
  Curl.set_url h "file:///dev/null";
  Curl.set_writefunction h String.length;
  h

let expect_ok code =
  if code <> Curl.CURLE_OK then
    fail ("unexpected curl result: " ^ Curl.strerror code)

let expect_invalid_arg f =
  match f () with
  | exception Invalid_argument _ -> ()
  | exception exn -> fail ("unexpected exception: " ^ Printexc.to_string exn)
  | _ -> fail "expected Invalid_argument"

let () =
  let pool = Curl_lwt.Pool.create () in
  Curl_lwt.Pool.setopt pool (Curl.Multi.CURLMOPT_MAXCONNECTS 2);
  let h = easy () in
  expect_ok (Lwt_main.run (Curl_lwt.Pool.perform pool h));
  expect_ok (Lwt_main.run (Curl_lwt.Pool.perform pool h));
  Curl_lwt.Pool.cleanup pool;
  Curl_lwt.Pool.cleanup pool;
  expect_invalid_arg (fun () -> Curl_lwt.Pool.perform pool h);
  expect_invalid_arg
    (fun () ->
      Curl_lwt.Pool.setopt pool (Curl.Multi.CURLMOPT_MAXCONNECTS 1));
  Curl.cleanup h;

  let h = easy () in
  let transfer = Curl_lwt.Pool.with_pool (fun pool ->
    Curl_lwt.Pool.perform pool h)
  in
  expect_ok (Lwt_main.run transfer);
  Curl.cleanup h;

  let pool = Curl_lwt.Pool.create () in
  let h = easy () in
  let transfer = Curl_lwt.Pool.perform pool h in
  Curl_lwt.Pool.cleanup pool;
  begin match Lwt.state transfer with
  | Lwt.Fail Lwt.Canceled -> ()
  | Lwt.Fail exn -> fail ("unexpected cancellation exception: " ^ Printexc.to_string exn)
  | Lwt.Return _ -> fail "pool cleanup completed an active transfer"
  | Lwt.Sleep -> fail "pool cleanup left an active transfer sleeping"
  end;
  Curl.cleanup h
