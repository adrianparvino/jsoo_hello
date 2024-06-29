module type Handler = sig
  module Response : sig
    type t

    val render : t -> Js.String.t
  end

  val handle :
    Headers.t ->
    Http_request.t ->
    Js.String.t Js.Dict.t ->
    Response.t Js.Promise.t
end

module Response = struct
  type t
  type options = { headers : Headers.t } [@@warning "-69"]

  external make : 'a -> options -> t = "Response" [@@mel.new]

  let create response =
    let headers = Headers.empty () in
    headers |> Headers.set "content-type" "application/json";
    make response { headers }
end

module Workers_request = struct
  type t = { _method : String.t; [@mel.as "method"] headers : Headers.t }

  external text : unit -> String.t Js.Promise.t = "text" [@@mel.send.pipe: t]
  external json : unit -> 'a Js.t Js.Promise.t = "json" [@@mel.send.pipe: t]
end

module Event = struct
  type t = { request : Workers_request.t }

  external respondWith : Response.t Js.Promise.t -> unit = "respondWith"
  [@@mel.send.pipe: t]
end

module Make (Handler : Handler) = struct
  external addEventListener : string -> (Event.t -> unit) -> unit
    = "addEventListener"

  let handle request env _ =
    let open Workers_request in
    let open Promise_utils.Bind in
    let headers = request.headers in
    let+ r =
      let open Promise_utils.Bind in
      match request._method with
      | "HEAD" -> Handler.handle headers Http_request.Head env
      | "GET" -> Handler.handle headers Http_request.Get env
      | "POST" ->
          let* body = request |> Workers_request.text () in
          Handler.handle headers (Http_request.Post body) env
      | "PUT" ->
          let* body = request |> Workers_request.text () in
          Handler.handle headers (Http_request.Put body) env
      | _ -> failwith "method not supported"
    in

    r |> Handler.Response.render |> Response.create

  let attachListener () =
    ignore
      (addEventListener "fetch" (fun (e : Event.t) ->
           e
           |> Event.respondWith
                (let request = e.request in
                 handle request [%mel.raw "globalThis"] ())))
end
