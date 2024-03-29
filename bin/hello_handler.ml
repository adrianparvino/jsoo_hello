module Response = struct
  type t = string

  let render s = s
end

let handle _ = "Hello from OCaml"
