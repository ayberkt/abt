open View
open Operator

module type ABT_PRINTER = sig
  module View : VIEW

  val printVar : Prettiest.t -> Prettiest.t
  val printAbs : Prettiest.t -> Prettiest.t -> Prettiest.t
  val printApp : View.op -> Prettiest.t list -> Prettiest.t
  val print : Prettiest.t -> string
end

module type OPERATOR_PRINTER = sig
  type op

  val print : op -> string
end

module MakeSexprPrinter (O : OPERATOR_PRINTER) (V : VIEW with type op = O.op) : 
  ABT_PRINTER with type View.op = V.op = struct
  open Prettiest
  open Prettiest.Infix

  module Option = Base.Option
  module View = V

  let printVar s = s
  let printAbs a b = a <> text " . " <> b
  let printApp op args =
    let prettyArgs = match args with [] -> empty | a -> text " " <> (sep args) in
    text "(" <> (text (O.print op)) <> prettyArgs <> text ")"

  let print e = Option.value ~default:"did not fit" (Prettiest.render 80 e)
end