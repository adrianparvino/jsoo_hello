open Js_of_ocaml

type t =
  | Head
  | Get
  | Post : Js.js_string Js.t -> t
  | Put : Js.js_string Js.t -> t
