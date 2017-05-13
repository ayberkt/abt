open Variable
open Operator
module CL = Core_kernel.Core_list

module type ABT = sig
    type sort
    type op
    type t
    module Variable : VARIABLE with type sort = sort

    type 'a view =
      VarView of Variable.t
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

    val to_string : t -> string

end

module MakeAbt (O : OPERATOR) : ABT = struct
  type sort = O.St.t
  type op = O.op
  module Variable : (VARIABLE with type sort = sort) = MakeVar(O.St)

  type t =
      FV of Variable.t
    | BV of Variable.t
    | ABS of sort * t
    | OPER of op * t list

  type 'a view =
    VarView of Variable.t
  | AbsView of Variable.t * 'a
  | AppView of op * 'a list

  exception Malformed

  (* Check that an abt has the right number of abstractions of right sort. *)
  let valence_ok (v : O.valence) e : bool =
    let rec valence_ok' srts e : bool =
      match (srts, e) with
      | [], (ABS _) -> false
      | s::ss, ABS (absrt, e') ->
          absrt = s && valence_ok' ss e'
      | _, (FV _ | BV _ | OPER _) -> true
    in valence_ok' (fst v) e

  let rec match_sorts (abts : t list) (vlncs : O.valence list) =
    match (abts, vlncs) with
    | [], [] -> true
    | [], _  -> false
    | _, []  -> false
    | ab::abs, v::vs -> snd v = get_sort ab && valence_ok v ab
  and get_sort (abt : t) : sort =
    match abt with
    | FV x -> Variable.vsort x
    | BV x -> Variable.vsort x
    | ABS _ -> raise Malformed
    | OPER (f, xs) -> failwith "induction"

  let into = failwith "foo"
  let out = failwith "foo"

  let aequiv = failwith "foo"
  let map = failwith "foo"

  let freevars = failwith "foo"
  let subst = failwith "foo"

  let intoVar = failwith "foo"
  let intoAbs = failwith "foo"
  let intoApp = failwith "foo"

  let ( !! ) = failwith "foo"
  let ( ^^ ) = failwith "foo"
  let ( $$ ) = failwith "foo"

  let to_string = failwith "foo"
end
(*  (ABT with type op := O.op and module Variable = Var) = struct
  open Util
  (* module Variable = MakeVar(O.S) *)

  type 'a view =
      VarView of Var.t
    | AbsView of Var.t * 'a
    | AppView of O.op * 'a list

  exception Malformed

  let map f = function
    | VarView x -> VarView x
    | AbsView (x, a) -> AbsView (x, f a)
    | AppView (h, vs) -> AppView(h, List.map f vs)

  (* let rec valence_ok (n, e) : bool =
    match e with
    | ABS e' -> if n > 0 then valence_ok (n-1, e') else false
    | (FV _ | BV _ |OPER _) -> if n = 0 then true else false *)

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
        if CL.for_all (Util.zip_exact Malformed (O.ar f) es) ~f:valence_ok
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

    module CL = Core_kernel.Core_list

    let rec freevars e =
      match out e with
        (* freevars with a var view consists only with itself *)
      | VarView x -> [x]
        (* freevars with f applied to es, are the unique freevars
        in all with e1, e2,..., en. *)
      | AppView (_, es) ->
            Util.collate Variable.equal (List.map freevars es)
        (* Free vars with an abstraction view are the ones in the body,
           except the variable `z` which is the one being abstracted.*)
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
          | VarView y -> if Variable.equal(x, y)
                     then out e
                     else VarView y
          | AppView (f, args) -> AppView (f, List.map (subst e x) args)
          | AbsView (z, arg) -> AbsView (z, subst e x arg)
        in
            into body'

      let rec to_string e =
        match out e with
        | VarView x -> Variable.to_string x
        | AppView (f, es) ->
            let esStr = if (CL.is_empty es) then "" else "(" ^ (toStrings es) ^ ")" in
              O.to_string f ^ esStr
         | AbsView (x, e) -> (Variable.to_string x) ^ "." ^ (to_string e)
      and toStrings = function
        | [] -> ""
        | [e] -> to_string e
        | e::es -> (to_string e) ^ "; " ^ (toStrings es)

end *)
