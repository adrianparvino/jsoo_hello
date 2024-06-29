type t

external empty : unit -> t = "Headers" [@@mel.new]
external get : string -> string option = "get" [@@mel.send.pipe: t]
external set : string -> string -> unit = "set" [@@mel.send.pipe: t]
