open Js_of_ocaml

module type Handler = sig
  module Response : sig
    type t

    val render : t -> Js.js_string Js.t
  end

  val handle : Headers.t -> Http_request.t -> Response.t Promise.t
end

module Response = struct
  type t

  let create body : t Js.t =
    let c : (Js.js_string Js.t -> _ Js.t -> t Js.t) Js.constr =
      Js.Unsafe.global ##. Response
    in
    let headers = Headers.(empty () |> set "content-type" "application/json") in
    new%js c
      body
      (object%js
         val headers = headers
      end)
end

module Make (Handler : Handler) = struct
  class type workers_request = object
    method _method : Js.js_string Js.t Js.readonly_prop
    method headers : Headers.t Js.readonly_prop
    method text : Js.js_string Js.t Promise.t Js.meth
    method json : 'a Js.t Promise.t Js.meth
  end

  class type event = object
    method request : workers_request Js.t Js.readonly_prop
    method respondWith : Response.t Js.t Promise.t -> unit Js.meth
  end

  let addEventListener (e : string) (cb : event Js.t -> unit) : unit =
    Js.Unsafe.fun_call
      Js.Unsafe.global##.addEventListener
      [| e |> Js.bytestring |> Js.Unsafe.inject; cb |> Js.Unsafe.inject |]

  let run () =
    ignore
      (addEventListener "fetch" (fun (e : event Js.t) ->
           let request = e##.request in
           let headers = request##.headers in
           let pr =
             let open Promise.Bind in
             match Js.to_bytestring request##._method with
             | "HEAD" -> Handler.handle headers Http_request.Head
             | "GET" -> Handler.handle headers Http_request.Get
             | "POST" ->
                 let* body = request##text in
                 Handler.handle headers (Http_request.Post body)
             | "PUT" ->
                 let* body = request##text in
                 Handler.handle headers (Http_request.Put body)
             | _ -> failwith "method not supported"
           in

           e##respondWith
             (pr
             |> Promise.map (fun r ->
                    r |> Handler.Response.render |> Response.create))))
end
