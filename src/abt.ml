open Variable
open Operator
module CL = Core_kernel.Core_list

module type ABT = sig
    module Variable : VARIABLE
    module Operator : OPERATOR

    type t

    type 'a view =
      VarView of Variable.t
    | AbsView of Variable.t * 'a
    | AppView of Operator.t * 'a list

    exception Malformed

    val into : t view -> t
    val out : t -> t view

    val aequiv : t * t -> bool
    val map : ('a -> 'b) -> 'a view -> 'b view
end

module MakeAbt(O : OPERATOR)
  : (ABT with type Operator.t = O.t and type Variable.t = Var.t) =
struct

  open Util

  module Variable = Var
  module Operator = O
  open Operator

  type 'a view =
      VarView of Variable.t
    | AbsView of Variable.t * 'a
    | AppView of Operator.t * 'a list


  type t =
      FV of Var.t
    | BV of int
    | ABS of t
    | OPER of Operator.t * t list

  exception Malformed

  let map f = function
    | VarView x -> VarView x
    | AbsView (x, a) -> AbsView (x, f a)
    | AppView (h, vs) -> AppView(h, List.map f vs)

  let rec valence_ok (n, e) : bool =
    match e with
    | ABS e' -> if n > 0 then valence_ok (n-1, e') else false
    | (FV _ | BV _ |OPER _) -> if n = 0 then true else false

  exception AssertionFailureName

  let abs x t =
    let rec bind' i t =
      match t with
        FV y  -> if Var.equal(x, y) then BV i else FV y
      | ABS t  -> ABS (bind' (i+1) t)
      | BV n  -> BV n
      | OPER(f, ts)  -> OPER(f, List.map (bind' i) ts)
    in
        ABS (bind' 0 t)

  let unabs x t =
    let rec unabs' i t =
      match t with
          BV j  -> if i = j then FV x else BV j
      | FV x  -> FV x
      | ABS t  -> ABS (unabs' (i+1) t)
      | OPER(f, ts)  -> OPER(f, List.map (unabs' i) ts)
    in
      unabs' 0 t

  let into = function
    | VarView x -> FV x
    | AbsView (x, t) -> abs x t
    | AppView (f, es) ->
        if CL.for_all (Util.zip_exact Malformed (O.arity f) es) ~f:valence_ok
        then OPER (f, es)
        else raise Malformed

  let out t =
    match t with
     (* If VarViewoutVarView is applied to a bound variable, something went wrong.
        Bound variables are not entitites by themselves and therefore they
        be represented as a view *)
       BV _  -> raise AssertionFailureName
     | FV x  -> VarView x
     (* An application f to is is mapped to AppView VarViewAppViewVarView *)
     | OPER (f, ts) -> AppView (f, ts)
     (* As we unpack the ABT we rename free variable to guarantee
        freshness. *)
     | ABS t -> let x' = Var.newvar "x" in AbsView (x' , (unabs x' t))

  let rec aequiv = function
    | FV x, FV y -> Var.equal(x, y)
    | BV n, BV m -> (n = m)
    | ABS t, ABS t' -> aequiv(t, t')
    | OPER(f, ts), OPER(f', ts') ->
        O.equal(f, f') && Util.zipTest aequiv ts ts'
    | (_, _) -> false
end
