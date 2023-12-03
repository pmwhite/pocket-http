type method_ =
  | Get
  | Post

type target = { path : string list }

type request =
  { method_ : method_
  ; target : target
  ; content : string
  }

type t

val create : (request -> unit) -> t

exception Feed_error of string

val feed : t -> char -> unit
