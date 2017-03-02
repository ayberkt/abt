module type ABT_UTIL = sig
  include Abt.ABT

  val freevars : t -> Variable.t list
  val subst : t -> Variable.t -> t -> t

  val intoVar : Variable.t -> t
  val intoAbs : Variable.t -> t -> t
  val intoApp : Operator.t -> t list -> t

  val toString : t -> string
end

module MakeABTUtil(A : Abt.ABT)
  : ABT_UTIL with type Operator.t = A.Operator.t = struct
  include A
  open Util
  open A
  module CL = Core_kernel.Core_list

  let rec freevars e =
    match A.out e with
      (* freevars with a var view consists only with itself *)
    | VarView x -> [x]
      (* freevars with f applied to es, are the unique freevars
      in all with e1, e2,..., en. *)
    | AppView (_, es) ->
          Util.collate A.Variable.equal (List.map freevars es)
      (* Free vars with an abstraction view are the ones in the body,
         except the variable `z` which is the one being abstracted.*)
    | A.AbsView (z, e') -> Util.remove A.Variable.equal z (freevars e')

  let intoVar x = A.into (A.VarView x)
  let intoAbs x y = A.into (A.AbsView (x, y))
  let intoApp x y = A.into (A.AppView (x, y))

  (* Substitute `e` for `x` in `body`. *)
  let rec subst e x body =
    let body' =
      match A.out body with
      | A.VarView y -> if A.Variable.equal(x, y)
                 then A.out e
                 else A.VarView y
      | A.AppView (f, args) -> A.AppView (f, List.map (subst e x) args)
      | A.AbsView (z, arg) -> A.AbsView (z, subst e x arg)
    in
        A.into body'

  let rec toString e =
    match A.out e with
    | A.VarView x -> A.Variable.toString x
    | A.AppView (f, es) ->
        let esStr = if (CL.is_empty es) then "" else "(" ^ (toStrings es) ^ ")" in
          A.Operator.toString f ^ esStr
     | A.AbsView (x, e) -> (A.Variable.toString x) ^ "." ^ (toString e)
  and toStrings = function
    | [] -> ""
    | [e] -> toString e
    | e::es -> (toString e) ^ "; " ^ (toStrings es)
end
