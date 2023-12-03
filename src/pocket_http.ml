type method_ =
  | Get
  | Post

type target = { path : string list }

type request =
  { method_ : method_
  ; target : target
  ; content : string
  }

type aux =
  { buffer : Buffer.t
  ; on_request : request -> unit
  ; mutable method_ : method_
  ; mutable path : string list
  ; mutable content_length : int
  }

type state =
  | Method_first
  | Method_rest
  | Space_after_method_first
  | Space_after_method_rest
  | Path_element_first
  | Path_element_rest
  | Space_after_target_first
  | Space_after_target_rest
  | Version_T1
  | Version_T2
  | Version_P
  | Version_Slash
  | Version_11
  | Version_Dot
  | Version_12
  | First_newline
  | First_newline_after_cr
  | Field_name_first
  | Field_name_rest
  | Field_value
  | Space_before_content_length
  | Content_length
  | Space_after_content_length
  | Header_newline
  | Newline_before_message_body
  | Message_body

let state_to_string (state : state) =
  match state with
  | Method_first -> "Method_first"
  | Method_rest -> "Method_rest"
  | Space_after_method_first -> "Space_after_method_first"
  | Space_after_method_rest -> "Space_after_method_rest"
  | Path_element_first -> "Path_element_first"
  | Path_element_rest -> "Path_element_rest"
  | Space_after_target_first -> "Space_after_target_first"
  | Space_after_target_rest -> "Space_after_target_rest"
  | Version_T1 -> "Version_T1"
  | Version_T2 -> "Version_T2"
  | Version_P -> "Version_P"
  | Version_Slash -> "Version_Slash"
  | Version_11 -> "Version_11"
  | Version_Dot -> "Version_Dot"
  | Version_12 -> "Version_12"
  | First_newline -> "First_newline"
  | First_newline_after_cr -> "First_newline_after_cr"
  | Field_name_first -> "Field_name_first"
  | Field_name_rest -> "Field_name_rest"
  | Field_value -> "Field_value"
  | Space_before_content_length -> "Space_before_content_length"
  | Content_length -> "Content_length"
  | Space_after_content_length -> "Space_after_content_length"
  | Header_newline -> "Header_newline"
  | Newline_before_message_body -> "Newline_before_message_body"
  | Message_body -> "Message_body"
;;

exception Step_error

let step_error () = raise Step_error

let rec step (state : state) (aux : aux) (c : char) : state =
  match state with
  | Method_first ->
    (match c with
     | 'A' .. 'Z' ->
       Buffer.add_char aux.buffer c;
       Method_rest
     | _ -> step_error ())
  | Method_rest ->
    (match c with
     | 'A' .. 'Z' ->
       Buffer.add_char aux.buffer c;
       Method_rest
     | _ ->
       (match Buffer.contents aux.buffer with
        | "GET" -> aux.method_ <- Get
        | "POST" -> aux.method_ <- Post
        | _ -> step_error ());
       Buffer.clear aux.buffer;
       step Space_after_method_first aux c)
  | Space_after_method_first ->
    (match c with
     | ' ' | '\t' -> Space_after_method_rest
     | _ -> step_error ())
  | Space_after_method_rest ->
    (match c with
     | ' ' | '\t' -> Space_after_method_rest
     | '/' -> Path_element_first
     | _ -> step_error ())
  | Path_element_first ->
    (match c with
     | 'a' .. 'z' | '0' .. '9' | '-' ->
       Buffer.add_char aux.buffer c;
       Path_element_rest
     | _ -> step_error ())
  | Path_element_rest ->
    (match c with
     | 'a' .. 'z' | '0' .. '9' | '-' ->
       Buffer.add_char aux.buffer c;
       Path_element_rest
     | '/' ->
       aux.path <- Buffer.contents aux.buffer :: aux.path;
       Buffer.clear aux.buffer;
       Path_element_first
     | _ ->
       aux.path <- Buffer.contents aux.buffer :: aux.path;
       Buffer.clear aux.buffer;
       step Space_after_target_first aux c)
  | Space_after_target_first ->
    (match c with
     | ' ' | '\t' -> Space_after_target_rest
     | _ -> step_error ())
  | Space_after_target_rest ->
    (match c with
     | ' ' | '\t' -> Space_after_target_rest
     | 'H' -> Version_T1
     | _ -> step_error ())
  | Version_T1 ->
    (match c with
     | 'T' -> Version_T2
     | _ -> step_error ())
  | Version_T2 ->
    (match c with
     | 'T' -> Version_P
     | _ -> step_error ())
  | Version_P ->
    (match c with
     | 'P' -> Version_Slash
     | _ -> step_error ())
  | Version_Slash ->
    (match c with
     | '/' -> Version_11
     | _ -> step_error ())
  | Version_11 ->
    (match c with
     | '1' -> Version_Dot
     | _ -> step_error ())
  | Version_Dot ->
    (match c with
     | '.' -> Version_12
     | _ -> step_error ())
  | Version_12 ->
    (match c with
     | '1' -> First_newline
     | _ -> step_error ())
  | First_newline ->
    (match c with
     | '\n' -> Field_name_first
     | '\r' -> First_newline_after_cr
     | _ -> step_error ())
  | First_newline_after_cr ->
    (match c with
     | '\n' -> Field_name_first
     | _ -> step_error ())
  | Field_name_first ->
    (match c with
     | 'a' .. 'z' | 'A' .. 'Z' | '-' ->
       Buffer.add_char aux.buffer (Char.lowercase_ascii c);
       Field_name_rest
     | '\r' -> Newline_before_message_body
     | '\n' -> Message_body
     | _ -> step_error ())
  | Field_name_rest ->
    (match c with
     | 'a' .. 'z' | 'A' .. 'Z' | '-' ->
       Buffer.add_char aux.buffer (Char.lowercase_ascii c);
       Field_name_rest
     | ':' ->
       let name = Buffer.contents aux.buffer in
       Buffer.clear aux.buffer;
       (match name with
        | "content-length" -> Space_before_content_length
        | _ -> Field_value)
     | _ -> step_error ())
  | Field_value ->
    (match c with
     | '\r' -> Header_newline
     | '\n' -> Field_name_first
     | _ -> Field_value)
  | Space_before_content_length ->
    (match c with
     | ' ' | '\t' -> Space_before_content_length
     | '0' .. '9' ->
       aux.content_length <- Char.code c - Char.code '0';
       Content_length
     | _ -> step_error ())
  | Content_length ->
    (match c with
     | '0' .. '9' ->
       if aux.content_length < 1000000
       then (
         aux.content_length <- (aux.content_length * 10) + (Char.code c - Char.code '0');
         Content_length)
       else step_error ()
     | ' ' | '\t' -> Space_after_content_length
     | '\r' -> Header_newline
     | '\n' -> Field_name_first
     | _ -> step_error ())
  | Space_after_content_length ->
    (match c with
     | ' ' | '\t' -> Space_after_content_length
     | '\r' -> Header_newline
     | '\n' -> Field_name_first
     | _ -> step_error ())
  | Header_newline ->
    (match c with
     | '\n' -> Field_name_first
     | _ -> step_error ())
  | Newline_before_message_body ->
    (match c with
     | '\n' -> Message_body
     | _ -> step_error ())
  | Message_body ->
    if aux.content_length > 0
    then (
      aux.content_length <- aux.content_length - 1;
      Buffer.add_char aux.buffer c;
      Message_body)
    else (
      aux.on_request
        { method_ = aux.method_
        ; target = { path = aux.path }
        ; content = Buffer.contents aux.buffer
        };
      aux.path <- [];
      aux.content_length <- 0;
      Buffer.clear aux.buffer;
      Method_first)
;;

type t =
  { mutable state : state
  ; aux : aux
  }

let create on_request =
  { state = Method_first
  ; aux =
      { on_request
      ; path = []
      ; content_length = 0
      ; buffer = Buffer.create (1024 * 1024)
      ; method_ = Get
      }
  }
;;

exception Feed_error of string

let feed t c =
  try t.state <- step t.state t.aux c with
  | Step_error ->
    raise
      (Feed_error
         (Printf.sprintf
            "Request parsing failed on character '%c' in state '%s'"
            c
            (state_to_string t.state)))
;;
