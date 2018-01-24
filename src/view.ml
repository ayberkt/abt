open Variable
open Operator

module type VIEW = sig
  module Variable : VARIABLE

  type op

  type 'a view =
    VarView of Variable.t
  | AbsView of Variable.t * 'a
  | AppView of op * 'a list
end

module MakeView (O : OPERATOR) = struct
  module Variable = Var

  type op = O.t

  type 'a view =
    VarView of Variable.t
  | AbsView of Variable.t * 'a
  | AppView of op * 'a list
end