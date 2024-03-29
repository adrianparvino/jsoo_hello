open Js_of_ocaml

type 'a t

val resolve : 'a -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val bind : ('a -> 'b t) -> 'a t -> 'b t

module Bind: sig
  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
end
