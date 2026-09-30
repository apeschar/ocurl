let () =
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  let share = Curl.Share.create_ca_cache () in
  for _ = 1 to 1000 do
    let a = Curl.init () and b = Curl.init () in
    Curl.set_share a (Some share);
    Curl.set_share b (Some share);
    Curl.set_ca_cache_timeout a 0;
    Curl.set_share a (Some share);
    Curl.reset a;
    Curl.set_share a (Some share);
    Curl.set_share b None;
    Curl.cleanup a;
    Curl.cleanup b
  done;
  let retained =
    let cache = Curl.Share.create_ca_cache () in
    let handle = Curl.init () in
    Curl.set_share handle (Some cache);
    handle
  in
  Gc.compact ();
  Curl.set_share retained None;
  Curl.cleanup retained;
  (match Curl.set_share retained (Some share) with
  | () -> failwith "closed handle accepted a share"
  | exception Invalid_argument _ -> ());
  Gc.compact ()
