open Abt_inner

module type ABT_PRINTER = sig
  module Abt : ABT

  val print : Abt.t -> string
end

module type OPERATOR_PRINTER = sig
  type op

  val print : op -> string
end

module MakeSexprPrinter
  (A : ABT)
  (O : OPERATOR_PRINTER with type op = A.op)
  (W : Prettiest.Width)
  : ABT_PRINTER
  with module Abt = A
  = struct

  module Option = Base.Option

  module Abt = A
  module Variable = Abt.Variable
  module P = Prettiest.Make(W)

  open P.Infix
  open P.Characters

  let rec toPretty e =
    match Abt.out e with
    | VarView x -> P.text (Variable.toUserString x)
    | AbsView (x, e) -> P.text (Variable.toUserString x) <+> dot <+> toPretty e
    | AppView (f, es) ->
      let args = List.map toPretty es in
      let prettyArgs = P.sep (P.intersperse ~sep:space args) in
        lparen <> (P.text (O.print f)) <//> prettyArgs <> rparen

  let print e = Option.value ~default:"did not fit" (P.render (toPretty e))
end
