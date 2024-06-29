open Js.Promise

module Bind = struct
  let ( let* ) p f = then_ f p
  let ( let+ ) p f = then_ (fun x -> resolve (f x)) p
end
