module List = ListLabels

let phys_equal = ( = )
let ( = ) = Int.equal
let https = ref []
let conns = ref []
let remove = ref []
let printfn fmt = Printf.ksprintf print_endline fmt

let run_server ~on_request =
  let accepting_socket = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.set_nonblock accepting_socket;
  Unix.bind accepting_socket (Unix.ADDR_INET (Unix.inet_addr_any, 8080));
  Unix.listen accepting_socket 32;
  let bytes = Bytes.create 1024 in
  let rec loop () =
    printfn
      "waiting for %d open connections and accepting new connections"
      (List.length !conns);
    let selected, _, _ = Unix.select (accepting_socket :: !conns) [] [] 5.0 in
    ListLabels.iter selected ~f:(fun socket ->
      if phys_equal socket accepting_socket
      then (
        match Unix.accept socket with
        | exception Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) -> ()
        | conn, client ->
          print_endline "accepted connection";
          let http = Pocket_http.create (on_request client) in
          conns := conn :: !conns;
          https := (conn, http) :: !https)
      else (
        let bytes_read = Unix.recv socket bytes 0 1024 [] in
        if bytes_read = 0
        then (
          remove := socket :: !remove;
          print_endline "connection closed")
        else (
          printfn "got %d bytes" bytes_read;
          let http = List.assoc socket !https in
          try
            for i = 0 to bytes_read - 1 do
              Pocket_http.feed http (Bytes.get bytes i)
            done
          with
          | exn -> print_endline (Printexc.to_string exn))));
    let () =
      let remove = !remove in
      let should_keep conn = not (List.mem ~set:remove conn) in
      conns := List.filter !conns ~f:(fun conn -> should_keep conn);
      https := List.filter !https ~f:(fun (conn, _http) -> should_keep conn)
    in
    remove := [];
    loop ()
  in
  loop ()
;;

let () =
  run_server ~on_request:(fun _client { method_; target; content } ->
    let method_ =
      match method_ with
      | Get -> "GET"
      | Post -> "POST"
    in
    let target = String.concat "/" target.path in
    printfn
      "----- received request -----\n%s /%s\n%s\n----------------------------"
      method_
      target
      (String.escaped content))
;;
