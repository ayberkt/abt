open Variable
open Operator

module type ABT = sig
  module Variable : VARIABLE

  type op
  type t

  type 'a view =
  | VarView of Variable.t
  | AbsView of Variable.t * 'a
  | AppView of op * 'a list

  exception Malformed

  val into : t view -> t
  val out : t -> t view

  val aequiv : t * t -> bool
  val map : ('a -> 'b) -> 'a view -> 'b view

  val freevars : t -> Variable.t list
  val subst : t -> Variable.t -> t -> t

  val intoVar : Variable.t -> t
  val intoAbs : Variable.t -> t -> t
  val intoApp : op -> t list -> t

  val ( !! ) : Variable.t -> t
  val ( ^^ ) : Variable.t -> t -> t
  val ( $$ ) : op -> t list -> t
end

module MakeAbt (O : OPERATOR) : ABT
  with type op = O.t
  and module Variable = Var
  = struct
  open Util

  module BL = Base.List
  module Variable = Var

  type op = O.t
  type t =
    | FV of Var.t
    | BV of int
    | ABS of Var.t * t
    | OPER of O.t * t list

  type 'a view =
  | VarView of Variable.t
  | AbsView of Variable.t * 'a
  | AppView of O.t * 'a list

  exception Malformed

  let map f = function
    | VarView x -> VarView x
    | AbsView (x, a) -> AbsView (x, f a)
    | AppView (h, vs) -> AppView(h, List.map f vs)

  let rec valence_ok (n, e) : bool =
    match e with
    | ABS (_, e') -> if n > 0 then valence_ok (n - 1, e') else false
    | FV _ | BV _ | OPER _ -> n = 0

  exception AssertionFailureName

  let abs x t =
    let rec bind' i t =
      match t with
      | FV y -> if Var.equal (x, y) then BV i else FV y
      | ABS (x, e) -> ABS (x, bind' (i + 1) e)
      | BV n -> BV n
      | OPER(f, ts) -> OPER (f, List.map (bind' i) ts)
    in
      ABS (x, bind' 0 t)

  let unabs x t =
    let rec unabs' i t =
      match t with
      | BV j -> if i = j then FV x else BV j
      | FV x -> FV x
      | ABS (x, e) -> ABS (x, unabs' (i + 1) e)
      | OPER (f, ts) -> OPER (f, List.map (unabs' i) ts)
    in
      unabs' 0 t

  let into = function
    | VarView x -> FV x
    | AbsView (x, t) -> abs x t
    | AppView (f, es) ->
      if BL.for_all (Util.zip_exact Malformed (O.arity f) es) ~f:valence_ok
      then OPER (f, es)
      else raise Malformed

  let out t =
    match t with
      (*
        If VarViewoutVarView is applied to a bound variable, something went wrong.
        Bound variables are not entitites by themselves and therefore they
        be represented as a view
      *)
    | BV _ -> raise AssertionFailureName
    | FV x -> VarView x
      (* An application f to is is mapped to AppView VarViewAppViewVarView *)
    | OPER (f, ts) -> AppView (f, ts)
      (* As we unpack the ABT we rename free variable to guarantee freshness. *)
    | ABS (x, e) -> let x' = Var.clone x in AbsView (x', (unabs x' e))

  let rec aequiv = function
    | FV x, FV y -> Var.equal(x, y)
    | BV n, BV m -> n = m
    | ABS (_, e), ABS (_, e') -> aequiv(e, e')
    | OPER (f, ts), OPER (f', ts') -> O.equal(f, f') && Util.zipTest aequiv ts ts'
    | _ -> false

  let rec freevars e =
    match out e with
      (* freevars with a var view consists only with itself *)
    | VarView x -> [x]
      (* freevars with f applied to es, are the unique freevars
        in all with e1, e2,..., en. *)
    | AppView (_, es) -> Util.collate Variable.equal (List.map freevars es)
      (*
        Free vars with an abstraction view are the ones in the body,
        except the variable `z` which is the one being abstracted.
      *)
    | AbsView (z, e') -> Util.remove Variable.equal z (freevars e')

  let intoVar x = into (VarView x)
  let intoAbs x y = into (AbsView (x, y))
  let intoApp x y = into (AppView (x, y))

  let ( !! ) x = intoVar x
  let ( ^^ ) x y = intoAbs x y
  let ( $$ ) x y = intoApp x y

  (* Substitute `e` for `x` in `body`. *)
  let rec subst e x body =
    let body' =
      match out body with
      | VarView y -> if Variable.equal(x, y) then out e else VarView y
      | AppView (f, args) -> AppView (f, List.map (subst e x) args)
      | AbsView (z, arg) -> AbsView (z, subst e x arg)
    in
      into body'
end
