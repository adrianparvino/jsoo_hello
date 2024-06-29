open Jsoo_hello

module SubtleCrypto = struct
  type key

  external importKey :
    string ->
    Js.Typed_array.Uint8Array.t ->
    'a Js.t ->
    bool ->
    string array ->
    key Js.Promise.t = "importKey"
  [@@mel.scope "crypto", "subtle"]

  external verify :
    string ->
    key ->
    Js.Typed_array.Uint8Array.t ->
    Js.Typed_array.Uint8Array.t ->
    bool Js.Promise.t = "verify"
  [@@mel.scope "crypto", "subtle"]
end

module TextEncoder = struct
  type t
  type encodeIntoResult = { read : int; written : int }

  external make : unit -> t = "TextEncoder" [@@mel.new]

  external encode : string -> Js.Typed_array.Uint8Array.t = "encode"
  [@@mel.send.pipe: t]

  external encodeInto :
    string -> Js.Typed_array.Uint8Array.t -> encodeIntoResult = "encodeInto"
  [@@mel.send.pipe: t]
end

module RawInteraction = struct
  type option = { t : int; [@mel.as "type"] name : string; value : string }

  type data = {
    t : int; [@mel.as "type"]
    name : string;
    options : option array Option.t;
  }

  type t = { t : int; [@mel.as "type"] data : data Option.t }
end

module Interaction = struct
  type option = { name : string; value : string }
  type data = { name : string; options : option array }
  type t = Ping | ApplicationCommand of { data : data }

  let options_of_raw (raw : RawInteraction.option) =
    match raw with { name; value; _ } -> { name; value }

  let data_of_raw (raw : RawInteraction.data) =
    match raw with
    | { name; options; _ } ->
        let options =
          options |> Option.value ~default:[||] |> Array.map options_of_raw
        in
        { name; options }

  let of_raw (raw : RawInteraction.t) =
    match raw with
    | { t = 1; _ } -> Ping
    | { t = 2; data; _ } ->
        let data = data_of_raw (Option.get data) in
        ApplicationCommand { data }
    | _ -> failwith "Unexpected interaction"
end

let verify signature signature_timestamp pubkey body : bool Js.Promise.t =
  let open Promise_utils.Bind in
  let encoder = TextEncoder.make () in
  let algorithm = "NODE-ED25519" in
  let* key =
    SubtleCrypto.importKey "raw" (Hex.from_hex pubkey)
      [%mel.obj { name = algorithm; namedCurve = algorithm }]
      true [| "verify" |]
  in
  let signature = Hex.from_hex signature in
  let buffer = Js.Typed_array.Uint8Array.fromLength 65536 in
  let { TextEncoder.written; _ } =
    encoder |> TextEncoder.encodeInto signature_timestamp buffer
  in
  let length = written in
  let { TextEncoder.written; _ } =
    encoder
    |> TextEncoder.encodeInto body
         (Js.Typed_array.Uint8Array.subarray ~start:length buffer)
  in
  let length = length + written in
  let data = Js.Typed_array.Uint8Array.subarray ~end_:length buffer in
  SubtleCrypto.verify algorithm key signature data

module type Handler = sig
  module Response : sig
    type t

    val render : t -> Js.String.t
  end

  val handle : Interaction.t -> Response.t Js.Promise.t
end

module Make (Handler : Handler) : Workers.Handler = struct
  (* module Response = struct
    type t = Pong | Roll of int

    type payload_data = {
      t : int; [@mel.as "type"]
      content : Js.String.t option; [@mel.optional]
    }
    [@@deriving jsProperties] [@@warning "-69"]

    type payload = {
      t : int; [@mel.as "type"]
      data : payload_data option; [@mel.optional]
    }
    [@@deriving jsProperties] [@@warning "-69"]

    let render response =
      let response =
        match response with
        | Pong -> payload ~t:1 ()
        | Roll n ->
            let n = Random.int n + 1 in

            let content = Int.to_string n in
            let data = payload_data ~t:1 ~content () in
            payload ~t:4 ~data ()
      in

      Js.Json.stringifyAny response |> Option.get
  end *)
  module Response = Handler.Response

  let handle (headers : Headers.t) (request : Http_request.t)
      (env : Js.String.t Js.Dict.t) : Response.t Js.Promise.t =
    Random.self_init ();
    match request with
    | Http_request.Post body ->
        Js.Console.log body;
        let open Promise_utils.Bind in
        let raw_interaction : RawInteraction.t =
          Js.Json.deserializeUnsafe body
        in
        let signature =
          headers
          |> Headers.get "x-signature-ed25519"
          |> Option.value ~default:""
        in
        let signature_timestamp =
          headers
          |> Headers.get "x-signature-timestamp"
          |> Option.value ~default:""
        in
        let pubkey =
          env |. Js.Dict.get "DISCORD_PUBLIC_KEY" |> Option.value ~default:""
        in
        let* verified = verify signature signature_timestamp pubkey body in
        if not verified then failwith "awa";
        let interaction = Interaction.of_raw raw_interaction in
        Handler.handle interaction
    | _ -> failwith "Awa"
end
