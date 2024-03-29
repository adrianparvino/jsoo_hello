open Js_of_ocaml
open Jsoo_hello

module Response = struct
  type t = Pong | Roll

  let render response =
    let response =
      match response with
      | Pong ->
          Js._JSON##stringify
            (object%js
               val _type = 1
            end)
      | Roll ->
          let n = Random.int 20 in
          Js._JSON##stringify
            (object%js
               val _type = 4

               val data =
                 object%js
                   val content = Js._JSON##stringify n
                 end
            end)
    in

    response
  (* Firebug.console##debug (Js.to_bytestring response); *)
  (* Js.to_bytestring response *)
end

class type encodeIntoResult = object
  method read : int Js.prop
  method written : int Js.prop
end

class type textencoder = object
  method encode : Js.js_string Js.t -> Typed_array.uint8Array Js.t Js.meth

  method encodeInto :
    Js.js_string Js.t ->
    Typed_array.uint8Array Js.t ->
    encodeIntoResult Js.t Js.meth
end

let from_hex hex : Typed_array.uint8Array Js.t =
  let s = Hex.to_string (`Hex hex) in
  let arr =
    Js.array (Array.init (String.length s) (fun i -> String.get_int8 s i))
  in
  new%js Typed_array.uint8Array_fromArray arr

let verify signature signature_timestamp pubkey body : bool Js.t Promise.t =
  let open Promise.Bind in
  let crypto = Js.Unsafe.global##.crypto in
  let encoder : textencoder Js.t =
    let te = Js.Unsafe.global##._TextEncoder in
    new%js te
  in
  let algorithm = Js.bytestring "NODE-ED25519" in
  let* key =
    crypto##.subtle##importKey
      (Js.bytestring "raw") (from_hex pubkey)
      (object%js
         val name = Js.bytestring "NODE-ED25519"
         val namedCurve = "NODE-ED25519"
      end)
      (Js.bool true)
      (Js.array [| Js.bytestring "verify" |])
  in
  let signature = from_hex signature in
  let data =
    encoder##encode ((Js.bytestring signature_timestamp)##concat body)
  in
  crypto##.subtle##verify algorithm key signature data

type interaction_data = { t : int; [@jsobject.key "type"] name : string }
[@@deriving show, jsobject]

type interaction = {
  t : int; [@jsobject.key "type"]
  data : interaction_data option;
}
[@@deriving show, jsobject]

let handle (headers : Headers.t) (request : Http_request.t) :
    Response.t Promise.t =
  Random.self_init ();
  match request with
  | Http_request.Post body ->
      let open Promise.Bind in
      let interaction = Js._JSON##parse body |> interaction_of_jsobject in
      let { t; data } = Result.get_ok interaction in
      let signature = Headers.get headers "x-signature-ed25519" in
      let signature_timestamp = Headers.get headers "x-signature-timestamp" in
      let pubkey = Js.to_bytestring Js.Unsafe.global##._DISCORD_PUBLIC_KEY_ in
      let* verified = verify signature signature_timestamp pubkey body in
      if not (Js.to_bool verified) then failwith "awa";
      Promise.resolve
        (match (t, data) with
        | 1, _ -> Response.Pong
        | 2, Some { name = "roll" } -> Response.Roll
        | _ -> failwith "awa")
  | _ -> failwith "Awa"
