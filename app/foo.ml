open Core_kernel.Core_printf
open Core_kernel.Core_list
open Operator
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

  let to_string op =
    match op with
    | Zero -> "z"
    | Succ -> "S"
    | Let  -> "let"
    | Lam  -> "lam"
    | Ap   -> "ap"
end

module LamTerm = MakeAbt(LamOp)

open LamTerm
open Variable


(* lam(x.x) *)
let id =
  let x1 = newvar "x" in
  let x2 = newvar "x" in
  let x3 = newvar "x" in
  Lam $$ [x1 ^^ (Lam $$ [x2 ^^ (Lam $$ [x3 ^^ !! x3])])]

let s1 = Ap $$ [id; id]

let s2 =
  let (x1, x2) = (newvar "x", newvar "y") in
  Lam $$ [x1 ^^ (Lam $$ [x2 ^^ !! x1])]

let zero = Zero $$ []
let one  = Succ $$ [zero]

(* S(S(z)) *)
let two  = Succ $$ [one]

let _ =
  let pr x =
    try
      printf "%s\n" (LamTerm.to_string x)
    with
    | Malformed -> printf "Malformed ABT\n" in
  iter [zero; one; two; id; s1; s2] ~f:pr
