module Response: sig
  type t
type options
end

module Workers_request: sig
  type t = { _method : String.t; [@mel.as "method"] headers : Headers.t }

  external text : unit -> String.t Js.Promise.t = "text" [@@mel.send.pipe: t]
  external json : unit -> 'a Js.t Js.Promise.t = "json" [@@mel.send.pipe: t]
end

module type Handler = sig
  module Response: sig 
    type t

    val render: t -> Js.String.t
  end

  val handle: Headers.t -> Http_request.t -> Js.String.t Js.Dict.t -> Response.t Js.Promise.t
end

module Make(_: Handler): sig
  val attachListener: unit -> unit

  val handle : Workers_request.t -> Js.String.t Js.Dict.t -> unit ->  Response.t Js.Promise.t
end
