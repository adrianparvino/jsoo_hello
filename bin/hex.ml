let lut =
  Array.init 256 (fun x ->
      let x = Char.chr x in
      match x with
      | '0' -> 0
      | '1' -> 1
      | '2' -> 2
      | '3' -> 3
      | '4' -> 4
      | '5' -> 5
      | '6' -> 6
      | '7' -> 7
      | '8' -> 8
      | '9' -> 9
      | 'a' -> 10
      | 'b' -> 11
      | 'c' -> 12
      | 'd' -> 13
      | 'e' -> 14
      | 'f' -> 15
      | _ -> 0)

let from_hex : string -> Js.Typed_array.Uint8Array.t =
  let dict = Js.Dict.empty () in
  fun hex ->
    match Js.Dict.get dict hex with
    | Some x -> x
    | _ ->
        let length = String.length hex / 2 in
        let buffer = Js.Typed_array.Uint8Array.fromLength length in
        for i = 0 to length - 1 do
          let hi = String.get_uint8 hex (2 * i) in
          let lo = String.get_uint8 hex ((2 * i) + 1) in
          Js.Typed_array.Uint8Array.unsafe_set buffer i
            ((16 * Array.unsafe_get lut hi) + Array.unsafe_get lut lo)
        done;
        Js.Dict.set dict hex buffer;
        buffer
