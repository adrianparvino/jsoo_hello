open Js_of_ocaml

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

let from_hex hex : Typed_array.uint8Array Js.t =
  let buffer = new%js Typed_array.uint8Array (String.length hex / 2) in
  for i = 0 to buffer##.byteLength - 1 do
    let hi = String.get_int8 hex (2 * i) in
    let lo = String.get_int8 hex ((2 * i) + 1) in
    Js.array_set (Js.Unsafe.coerce buffer) i ((16 * lut.(hi)) + lut.(lo))
  done;
  buffer
