open Js_of_ocaml

type request = Http_request.t

module type Handler = sig
  module Response : sig
    type t

    val render : t -> string
  end

  val handle : request -> Response.t
end

module Promise : sig
  type 'a t

  val resolve : 'a -> 'a t Js.t
  val map : resolve:('a -> 'b) -> 'a t Js.t -> 'b t Js.t
end = struct
  type 'a t

  let promise = Js.Unsafe.global##._Promise
  let resolve x = promise##resolve x

  let map ~resolve p =
    Js.Unsafe.meth_call p "then" [| Js.Unsafe.inject resolve |]
end

module Make (Handler : Handler) = struct
  type response

  class type workers_request = object
    method _method : Js.js_string Js.t Js.readonly_prop
    method text : Js.js_string Js.t Promise.t Js.t Js.meth
    method json : 'a Js.t Promise.t Js.t Js.meth
  end

  class type event = object
    method request : workers_request Js.t Js.readonly_prop
    method respondWith : response Js.t Promise.t Js.t -> unit Js.meth
  end

  let response : (string -> response Js.t) Js.constr =
    Js.Unsafe.global ##. Response

  let addEventListener (e : string) (cb : event Js.t -> unit) : unit =
    Js.Unsafe.fun_call
      Js.Unsafe.global##.addEventListener
      [| e |> Js.bytestring |> Js.Unsafe.inject; cb |> Js.Unsafe.inject |]

  let run () =
    ignore
      (addEventListener "fetch" (fun (e : event Js.t) ->
           let request = e##.request in
           let pr =
             match Js.to_bytestring request##._method with
             | "GET" -> Promise.resolve (Handler.handle Http_request.Get)
             | "POST" ->
                 let promise = request##json in
                 promise
                 |> Promise.map ~resolve:(fun _ ->
                        Handler.handle Http_request.Get)
             | _ -> failwith "method not supported"
           in

           e##respondWith
             (pr
             |> Promise.map ~resolve:(fun x ->
                    new%js response (Handler.Response.render x)))))
end
