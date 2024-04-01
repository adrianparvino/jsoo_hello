open Js_of_ocaml
open Jsoo_hello

type encodeIntoResult = { read : int; written : int }
[@@deriving show, jsobject]

class type textencoder = object
  method encode : Js.js_string Js.t -> Typed_array.uint8Array Js.t Js.meth

  method encodeInto :
    Js.js_string Js.t ->
    Typed_array.uint8Array Js.t ->
    encodeIntoResult Js.t Js.meth
end

module Interaction = struct
  type interaction_data = { t : int; [@jsobject.key "type"] name : string }
  [@@deriving show, jsobject]

  type t = { t : int; [@jsobject.key "type"] data : interaction_data option }
  [@@deriving show, jsobject]
end

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
      (Js.bytestring "raw") (Hex.from_hex pubkey)
      (object%js
         val name = Js.bytestring "NODE-ED25519"
         val namedCurve = "NODE-ED25519"
      end)
      (Js.bool true)
      (Js.array [| Js.bytestring "verify" |])
  in
  let signature = Hex.from_hex signature in
  let buffer = new%js Typed_array.uint8Array 65536 in
  let { written } =
    encoder##encodeInto (Js.bytestring signature_timestamp) buffer
    |> encodeIntoResult_of_jsobject |> Result.get_ok
  in
  let length = written in
  let { written } =
    encoder##encodeInto body (buffer##subarray_toEnd length)
    |> encodeIntoResult_of_jsobject |> Result.get_ok
  in
  let length = length + written in
  let data = buffer##subarray 0 length in
  crypto##.subtle##verify algorithm key signature data
