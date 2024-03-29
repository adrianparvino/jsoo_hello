module type Handler = sig
  open Js_of_ocaml
  module Response: sig 
    type t

    val render: t -> Js.js_string Js.t
  end

  val handle: Headers.t -> Http_request.t -> Response.t Promise.t
end

module Make : functor (_: Handler) -> sig
  val run: unit -> unit
end
