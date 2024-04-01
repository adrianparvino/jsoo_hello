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
end

let handle (headers : Headers.t) (request : Http_request.t) :
    Response.t Promise.t =
  Random.self_init ();
  match request with
  | Http_request.Post body ->
      let open Promise.Bind in
      let interaction =
        Js._JSON##parse body |> Discord.Interaction.of_jsobject
      in
      let { Discord.Interaction.t; data } = Result.get_ok interaction in
      let signature = Headers.get headers "x-signature-ed25519" in
      let signature_timestamp = Headers.get headers "x-signature-timestamp" in
      let pubkey = Js.to_bytestring Js.Unsafe.global##._DISCORD_PUBLIC_KEY_ in
      let* verified =
        Discord.verify signature signature_timestamp pubkey body
      in
      if not (Js.to_bool verified) then failwith "awa";
      Promise.resolve
        (match (t, data) with
        | 1, _ -> Response.Pong
        | 2, Some { name = "roll" } -> Response.Roll
        | _ -> failwith "awa")
  | _ -> failwith "Awa"
