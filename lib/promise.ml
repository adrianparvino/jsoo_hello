open Js_of_ocaml

class type ['b] t_ = object
  method resolve : ('a -> 'b) -> 'b t_ Js.t Js.meth
end

type 'a t = 'a t_ Js.t

let promise = Js.Unsafe.global##._Promise
let resolve x = promise##resolve x
let map f p = Js.Unsafe.meth_call p "then" [| Js.Unsafe.inject f |]
let bind f p = Js.Unsafe.meth_call p "then" [| Js.Unsafe.inject f |]

module Bind = struct
  let ( let* ) p f = bind f p
end
