open Js_of_ocaml

class type t_ = object
  method get : Js.js_string Js.t -> Js.js_string Js.t Js.meth
  method set : Js.js_string Js.t -> Js.js_string Js.t -> t_ Js.t Js.meth
end

type t = t_ Js.t

let empty () : t =
  let c = Js.Unsafe.global##._Headers in
  new%js c

let get headers name = headers##get (Js.bytestring name) |> Js.to_bytestring

let set name value headers =
  headers##set (Js.bytestring name) (Js.bytestring value);
  headers
