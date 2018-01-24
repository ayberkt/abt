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
  : ABT_PRINTER
  with type View.op = V.op
  and type Abt.t = A.t
  = struct

  open Prettiest
  open Prettiest.Infix

  module Option = Base.Option

  module View = V
  module Abt = A

  open View

  type t = A.t

  let rec toPretty e =
    match A.out e with
    | VarView x -> text (Variable.toUserString x)
    | AbsView (x, e) -> text (Variable.toUserString x) <> text " . " <> toPretty e
    | AppView (f, es) ->
      let args = List.map toPretty es in
      let prettyArgs = match args with [] -> empty | args' -> text " " <> (sep args') in
      text "(" <> (text (O.print f)) <> prettyArgs <> text ")"
    
  let print e = Option.value ~default:"did not fit" (Prettiest.render 80 (toPretty e))
end