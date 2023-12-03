[@@@warning "-38-32-60-34-69"]

module Parser : sig
  type 'a parser

  type 'a parse_result =
    | Done of 'a
    | Peeked of 'a
    | Next of 'a parser

  val feed : 'a parser -> char -> 'a parse_result
  val char : char parser
  val peek : char parser
  val skip : unit parser
  val return : 'a -> 'a parser
  val unit : unit parser
  val ( let+ ) : 'a parser -> ('a -> 'b) -> 'b parser
  val ( let* ) : 'a parser -> ('a -> 'b parser) -> 'b parser
end = struct
  type 'a parse_result =
    | Done of 'a
    | Peeked of 'a
    | Next of 'a parser

  and 'a parser = char -> 'a parse_result

  let feed t c = t c
  let char c = Done c
  let peek c = Peeked c
  let skip _c = Done ()

  let rec bind t ~f c =
    match t c with
    | Done x -> Next (f x)
    | Peeked x -> f x c
    | Next t -> Next (bind t ~f)
  ;;

  let return x _c = Peeked x
  let unit _c = Peeked ()

  let rec map t ~f c =
    match t c with
    | Done x -> Done (f x)
    | Peeked x -> Peeked (f x)
    | Next tf -> Next (map tf ~f)
  ;;

  let ( let* ) t f = bind t ~f
  let ( let+ ) t f = map t ~f
end

exception Expected of string

let expected message = raise_notrace (Expected message)

type method_ =
  | Get
  | Post

type target = { path : string list }

type request =
  { method_ : method_
  ; target : target
  ; content : string
  }

open Parser

let is_token_char = function
  | '!' | '#' | '$' | '%' | '&' | '\'' -> true
  | '*' | '+' | '-' | '.' | '^' | '_' | '`' | '|' | '~'
  | '0' .. '9'
  | 'a' .. 'z'
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let buffer = Buffer.create (1024 * 1024)

let many1_satisfying name ~first ~rest : string parser =
  let* c = char in
  if first c
  then (
    let rec loop () =
      let* c = peek in
      if rest c
      then (
        let* () = skip in
        Buffer.add_char buffer c;
        loop ())
      else return ()
    in
    let* () = loop () in
    let result = Buffer.contents buffer in
    Buffer.clear buffer;
    return result)
  else expected name
;;

let rec parse_ows () : unit parser =
  let* c = peek in
  match c with
  | ' ' | '\t' ->
    let* () = skip in
    parse_ows ()
  | _ -> unit
;;

let parse_rws () : unit parser =
  let* c = char in
  match c with
  | ' ' | '\t' -> parse_ows ()
  | _ -> expected "space"
;;

let parse_bws () = parse_ows ()

let parse_space : unit parser =
  let+ c = char in
  match c with
  | ' ' -> ()
  | _ -> expected "space"
;;

let parse_newline =
  let* c = char in
  match c with
  | '\n' -> return ()
  | '\r' ->
    let* c = char in
    (match c with
     | '\n' -> return ()
     | _ -> expected "newline")
  | _ -> expected "newline"
;;

let parse_method =
  let f = function
    | 'A' .. 'Z' -> true
    | _ -> false
  in
  let+ s = many1_satisfying "method" ~first:f ~rest:f in
  match s with
  | "GET" -> Get
  | "POST" -> Post
  | _ -> expected "'GET' or 'POST'"
;;

let parse_path =
  let parse_path_element =
    many1_satisfying
      "path element"
      ~first:(function
        | 'a' .. 'z' -> true
        | '0' .. '9' | _ -> false)
      ~rest:(function
        | 'a' .. 'z' | '-' -> true
        | _ -> false)
  in
  let rec loop acc =
    let* c = peek in
    match c with
    | '/' ->
      let* element = parse_path_element in
      loop (element :: acc)
    | _ ->
      (match acc with
       | [] -> expected "path"
       | _ :: _ -> return (List.rev acc))
  in
  loop []
;;

let parse_request_target () : target parser =
  let+ path = parse_path in
  { path }
;;

let parse_protocol_version =
  let s = "HTTP/1.1" in
  let rec loop i =
    if i = String.length s
    then return ()
    else
      let* c = char in
      if Char.equal c s.[i] then loop (i + 1) else expected "'HTTP/1.1'"
  in
  loop 0
;;

let parse_request_line =
  let* method_ = parse_method in
  let* () = parse_rws () in
  let* target = parse_request_target () in
  let* () = parse_rws () in
  let* () = parse_protocol_version in
  let+ () = parse_newline in
  method_, target
;;

let parse_field_name =
  many1_satisfying
    "field name"
    ~first:(function
      | 'a' .. 'z' | 'A' .. 'Z' -> true
      | _ -> false)
    ~rest:(function
      | 'a' .. 'z' | 'A' .. 'Z' | '-' -> true
      | _ -> false)
;;

let parse_field_value =
  let rec loop i first last =
    let* c = peek in
    match c with
    | '\n' | '\r' -> return (first, last)
    | ' ' | '\t' ->
      let* () = skip in
      Buffer.add_char buffer c;
      loop (i + 1) first last
    | _ ->
      let* () = skip in
      Buffer.add_char buffer c;
      loop (i + 1) (Int.min first i) (Int.max last i)
  in
  let+ first, last = loop 0 Int.max_int 0 in
  let result = Buffer.contents buffer in
  Buffer.clear buffer;
  let first = if first < Int.max_int then first else 0 in
  String.sub result first (last - first)
;;

let parse_headers =
  let rec loop content_length =
    let* c = peek in
    match c with
    | '\r' | '\n' ->
      let+ () = parse_newline in
      content_length
    | _ ->
      let* name = parse_field_name in
      let* c = char in
      (match c with
       | ':' ->
         let* value = parse_field_value in
         let* () = parse_newline in
         (match content_length with
          | Some _ -> loop content_length
          | None ->
            (match String.lowercase_ascii name with
             | "content-length" ->
               (match int_of_string_opt value with
                | Some value ->
                  if value > 0 && value < 1024 * 1024
                  then loop (Some value)
                  else expected "valid content length"
                | None -> expected "valid content length")
             | _ -> loop content_length))
       | _ -> expected "colon")
  in
  loop None
;;

let rec parse_message_body length =
  if length = 0
  then (
    let result = Buffer.contents buffer in
    Buffer.clear buffer;
    return result)
  else
    let* c = char in
    Buffer.add_char buffer c;
    parse_message_body (length - 1)
;;

let parse_request : request parser =
  let* method_, target = parse_request_line in
  let* content_length = parse_headers in
  let* content =
    match content_length with
    | Some length -> parse_message_body length
    | None -> return ""
  in
  return { method_; target; content }
;;

type t =
  { mutable state : request parser
  ; on_request : request -> unit
  }

let create on_request = { state = parse_request; on_request }

let rec feed t c =
  match Parser.feed t.state c with
  | Done request ->
    t.state <- parse_request;
    t.on_request request
  | Peeked request ->
    t.state <- parse_request;
    t.on_request request;
    feed t c
  | Next p -> t.state <- p
;;
