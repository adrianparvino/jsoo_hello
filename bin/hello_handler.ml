open Jsoo_hello

module Tree = struct
  type t = Int of int | Repeat of int * int | Sum of t list | Negate of t

  let rec run tree =
    match tree with
    | Int n -> (n, Int.to_string n)
    | Repeat (m, n) ->
        let results = List.init m (fun _ -> Random.int n + 1) in
        ( List.fold_left ( + ) 0 results,
          String.concat " + " (List.map Int.to_string results) )
    | Sum (x :: xs) ->
        List.fold_left
          (fun (n, str) -> function
            | Negate x ->
                let n', str' = run x in
                (n - n', str ^ " - " ^ str')
            | x ->
                let n', str' = run x in
                (n + n', str ^ " + " ^ str'))
          (run x) xs
    | Negate x ->
        let n, str = run x in
        (-n, "-" ^ str)
    | _ -> failwith "Unexpected tree"

  let rec pp f = function
    | Int n -> Format.fprintf f "%d" n
    | Repeat (1, n) -> Format.fprintf f "d%d" n
    | Repeat (m, n) -> Format.fprintf f "%dd%d" m n
    | Sum (x :: xs) ->
        Format.fprintf f "%a" pp x;
        List.iter
          (function
            | Negate x -> Format.fprintf f " - %a" pp x
            | x -> Format.fprintf f " + %a" pp x)
          xs
    | Negate x -> Format.fprintf f "-%a" pp x
    | _ -> failwith "Unexpected tree"
end

module Response = struct
  type t = Pong | Command of Tree.t

  type payload_data = {
    t : int; [@mel.as "type"]
    content : Js.String.t option; [@mel.optional]
  }
  [@@deriving jsProperties]

  type payload = {
    t : int; [@mel.as "type"]
    data : payload_data option; [@mel.optional]
  }
  [@@deriving jsProperties]

  let render response =
    let response =
      match response with
      | Pong -> payload ~t:1 ()
      | Command tree ->
          let roll, str = Tree.run tree in
          let data =
            payload_data ~t:1 ~content:(str ^ " = " ^ Int.to_string roll) ()
          in
          payload ~t:4 ~data ()
    in
    Js.Json.stringifyAny response |> Option.get
end

module Lexer = struct
  module Token = struct
    type t = D of int | Int of int | Plus | Minus

    let pp f x =
      match x with
      | Int n -> Format.fprintf f "%d" n
      | D n -> Format.fprintf f "d%d" n
      | Plus -> Format.fprintf f "+"
      | Minus -> Format.fprintf f "-"
  end

  let run str =
    let pattern = [%re "/(?:(\\d+)|d(\\d+)?|\\+|-)/g"] in
    Seq.of_dispenser (fun () -> Js.Re.exec ~str pattern)
    |> Seq.map (fun x ->
           let x = Js.Re.captures x in
           match x with
           | [| _; x; _ |] when not (Js.Nullable.isNullable x) ->
               Token.Int
                 (x |> Js.Nullable.toOption |> Option.get |> int_of_string)
           | [| _; _; y |] when not (Js.Nullable.isNullable y) ->
               D (y |> Js.Nullable.toOption |> Option.get |> int_of_string)
           | [| s; _; _ |] when s == Js.Nullable.return "+" -> Plus
           | [| s; _; _ |] when s == Js.Nullable.return "-" -> Minus
           | _ -> failwith "Unexpected token")
end

module Parser : sig
  type t

  val create : unit -> t
  val run : Lexer.Token.t Seq.t -> t -> t
  val finalize : t -> Tree.t
end = struct
  type t =
    | Nil : t
    | Parsed : (Tree.t * t) -> t
    | Terminal : (Lexer.Token.t * t) -> t

  let create () = Nil

  let rec pp f = function
    | Nil -> Format.fprintf f "Nil"
    | Parsed (tree, state) ->
        Format.fprintf f "Parsed(%a, %a)" Tree.pp tree pp state
    | Terminal (token, state) ->
        Format.fprintf f "Terminal(%a, %a)" Lexer.Token.pp token pp state

  let step state token =
    match (state, token) with
    (* Convert numbers to expressions *)
    | Nil, Lexer.Token.Minus -> Terminal (Minus, Nil)
    | Terminal (Minus, Nil), Lexer.Token.Int n -> Parsed (Negate (Int n), state)
    | Terminal (Minus, Nil), Lexer.Token.D n ->
        Parsed (Negate (Repeat (1, n)), state)
    | state, Lexer.Token.Int n -> Parsed (Int n, state)
    | Parsed (Int m, state), D n -> Parsed (Repeat (m, n), state)
    | Parsed (Negate (Int m), state), D n ->
        Parsed (Negate (Repeat (m, n)), state)
    | state, D n -> Parsed (Repeat (1, n), state)
    (* Collapse consecutive sums *)
    | ( Parsed (expr, Terminal (Plus, Parsed (Sum xs, Nil))),
        ((Plus | Minus) as op) ) ->
        Terminal (op, Parsed (Sum (expr :: xs), Nil))
    | ( Parsed (expr, Terminal (Minus, Parsed (Sum xs, Nil))),
        ((Plus | Minus) as op) ) ->
        Terminal (op, Parsed (Sum (Negate expr :: xs), Nil))
    (* Promote literals to sums *)
    | Parsed (expr, Nil), Plus -> Terminal (Plus, Parsed (Sum [ expr ], Nil))
    | Parsed (expr, Nil), Minus -> Terminal (Minus, Parsed (Sum [ expr ], Nil))
    | _ -> failwith "Unexpected token"

  let run state tokens = Seq.fold_left step tokens state

  let rec reverse_trees = function
    | Tree.Sum xs -> Tree.Sum (xs |> List.map reverse_trees |> List.rev)
    | tree -> tree

  let rec finalize state =
    match state with
    | Parsed (tree, Nil) -> tree |> reverse_trees
    | Parsed (expr, Terminal (Plus, Parsed (Sum xs, state))) ->
        Parsed (Sum (expr :: xs), state) |> finalize
    | Parsed (expr, Terminal (Minus, Parsed (Sum xs, state))) ->
        Parsed (Sum (Negate expr :: xs), state) |> finalize
    | _ -> failwith "Unexpected end of input"
end

let handle (interaction : Discord.Interaction.t) : Response.t Js.Promise.t =
  Random.self_init ();
  let result =
    match interaction with
    | Ping -> Response.Pong
    | ApplicationCommand
        { data = { name = "roll"; options = [| { value; _ } |] } } ->
        let tokens = Lexer.run value in
        Command (Parser.create () |> Parser.run tokens |> Parser.finalize)
    | _ -> failwith "Unexpected interaction"
  in
  Js.Promise.resolve result
