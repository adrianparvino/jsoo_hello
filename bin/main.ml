let default =
  let open Jsoo_hello.Workers.Make (Discord.Make (Hello_handler)) in
  [%mel.obj { fetch = (fun x y z -> handle x y z) }]
