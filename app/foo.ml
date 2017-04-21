open Core_kernel.Core_printf;;
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
