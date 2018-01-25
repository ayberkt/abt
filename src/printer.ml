open View
open Operator
open Abt_inner

module type ABT_PRINTER = sig
  module Abt : ABT
  module View : VIEW

  val print : Abt.t -> string
end

module type OPERATOR_PRINTER = sig
  type op

  val print : op -> string
end

module MakeSexprPrinter
  (O : OPERATOR_PRINTER)
  (V : VIEW with type op = O.op)
  (A : ABT with type View.op = O.op and type View.Variable.t = V.Variable.t)
  (W : Prettiest.Width)
  : ABT_PRINTER
  with type View.op = V.op
  and type Abt.t = A.t
  = struct

  module Option = Base.Option

  module View = V
  module Abt = A
  module P = Prettiest.Make(W)

  open View
  open P.Infix

  type t = A.t

  let rec toPretty e =
    match A.out e with
    | VarView x -> P.text (Variable.toUserString x)
    | AbsView (x, e) -> P.text (Variable.toUserString x) <> P.text " . " <> toPretty e
    | AppView (f, es) ->
      let args = List.map toPretty es in
      let prettyArgs =
        match args with
        | [] -> P.empty
        | args' -> P.text " " <> (P.sep args') in
          P.text "(" <> (P.text (O.print f)) <> prettyArgs <> P.text ")"
    
  let print e = Option.value ~default:"did not fit" (P.render (toPretty e))
end