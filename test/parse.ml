let () =
  let input = In_channel.input_all In_channel.stdin in
  let parser = Pocket_http.create (fun _ -> print_endline "got request") in
  for i = 0 to String.length input - 1 do
    Pocket_http.feed parser input.[i]
  done
;;
