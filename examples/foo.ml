open Base.List
open Sort
open Operator
(* open Abt *)

type lam_sort =
    Decl
  | Expr

type lam_op =
    Zero
  | Succ
  | Let
  | Lam
  | Ap
  | FnDecl

module LamSort : (SORT with type t = lam_sort) = struct
  type t = lam_sort
  let eq x y = x = y

  let to_string = function
    | Decl -> "decl"
    | Expr -> "expr"
end

module LamOp : OPERATOR with type op = lam_op = struct
  module St = LamSort
  type op = lam_op

  type sort = St.t
  type valence = sort list * sort
  type arity   = valence list * sort

  let constant s = ([], s)
  let nobind s = ([], s)
  let (-->>) (xs : valence list) (s : sort) = (xs, s)
  let (>>>) xs s = (xs, s)

  let ar op =
    match op with
    | Zero -> constant Expr
    | Succ -> [[] >>> Expr] -->> Expr
    | Let  -> [[] >>> Expr; [Expr] >>> Expr] -->> Expr
    | Lam  -> [[Expr] >>> Expr] -->> Expr
    | Ap  -> [[] >>> Expr; [] >>> Expr] -->> Expr
    | FnDecl -> [[] >>> Expr; [Expr] >>> Decl] -->> Decl


  let equal (x, y) = x = y

  let to_string op =
    match op with
    | Zero -> "z"
    | Succ -> "S"
    | Let  -> "let"
    | Lam  -> "lam"
    | Ap   -> "ap"
    | FnDecl -> "decl"
end

module LamTerm = MakeAbt(LamOp)

open LamTerm
open Variable


(* lam(x.x) *)
let id =
  let x1 = named "x" in
  let x2 = named "x" in
  let x3 = named "x" in
  Lam $$ [x1 ^^ (Lam $$ [x2 ^^ (Lam $$ [x3 ^^ !! x3])])]

let s1 = Ap $$ [id; id]

let s2 =
  let (x1, x2) = (named "x", named "y") in
  Lam $$ [x1 ^^ (Lam $$ [x2 ^^ !! x1])]

let zero = Zero $$ []
let one  = Succ $$ [zero]

(* S(S(z)) *)
let two  = Succ $$ [one]

let _ =
  let pr x =
    try
    print_string (LamTerm.to_string x)
    with
    | Malformed -> print_string "Malformed ABT\n" in
  iter [zero; one; two; id; s1; s2] ~f:pr