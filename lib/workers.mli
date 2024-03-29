type request = Http_request.t

module type Handler = sig
  module Response: sig 
    type t

    val render: t -> string
  end

  val handle: request -> Response.t
end

module Make : functor (_: Handler) -> sig
  val run: unit -> unit
end
