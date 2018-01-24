open Base.List
open Abt

type lam_op =
    Zero
  | Succ
  | Let
  | Lam
  | Ap

module LamOp : OPERATOR with type t = lam_op = struct
  type t = lam_op

  let arity op =
    match op with
    | Zero -> []
    | Succ -> [0]
    | Let  -> [0; 1]
    | Lam  -> [1]
    | Ap  -> [0; 0]

  let equal (x, y) =
    match (x, y) with
    | Zero, Zero -> true
    | Succ, Succ -> true
    | Let, Let   -> true
    | Lam, Lam   -> true
    | Ap, Ap     -> true
    | _          -> false
end

module LamOpPrinter : OPERATOR_PRINTER with type op = lam_op = struct
  type op = lam_op

  let print op =
    match op with
    | Zero -> "zero"
    | Succ -> "succ"
    | Let -> "let"
    | Lam -> "lam"
    | Ap -> "ap"
end

module LamTerm = MakeAbt(LamOp)
module LamPrinter = MakeSexprPrinter(LamOpPrinter)(MakeView(LamOp))(LamTerm)

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
    print_string (LamPrinter.print x);
    print_newline();
    with
    | Malformed -> print_string "Malformed ABT\n" in
  iter [zero; one; two; id; s1; s2] ~f:pr