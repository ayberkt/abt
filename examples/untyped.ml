open Abt
open List

(* An interpreter for untyped lambda calculus *)

type untyped_op =
    Lam
  | Ap

module UntypedOp : OPERATOR with type t = untyped_op = struct
  type t = untyped_op

  let arity op =
    match op with
    | Lam -> [1]
    | Ap -> [0; 0]

  let equal (x, y) =
    match (x, y) with
    | Lam, Lam -> true
    | Ap, Ap -> true
    | _ -> false
end

module UntypedTerm = MakeAbt(UntypedOp)

module UntypedPrinter : ABT_PRINTER
  with module Abt = UntypedTerm
  = struct

  module Option = Base.Option

  module Abt = UntypedTerm
  module Variable = Abt.Variable
  module P = Prettiest.Make(struct let width = 80 end)

  open P.Infix
  open P.Characters

  let rec toPretty e =
    match Abt.out e with
    | VarView x -> P.text (Variable.toUserString x)
    | AbsView (x, e) -> P.text (Variable.toUserString x) <+> dot <+> toPretty e
    | AppView (f, es) ->
      let args = List.map toPretty es in
      let prettyArgs = P.sep (P.intersperse space args) in
      begin
        match f with
        | Lam -> lparen <> P.text "λ" <//> prettyArgs <> rparen
        | Ap -> lparen <> prettyArgs <> rparen
      end

  let print e = Option.value ~default:"did not fit" (P.render (toPretty e))
end

open UntypedTerm
open Variable

(* call-by-value *)
let rec eval e =
  into (
    match out e with
    (* beta reduction *)
    | AppView (Ap, [a; y]) ->
        (match out a with
        | AppView (Lam, [a]) ->
            (match out a with
            | AbsView (x, e) -> out (eval (subst y x e));
            | x -> x)
        | AppView (_, _) ->
          let eval_a = eval a in
          let eval_y = eval y in
            out (eval (Ap $$ [eval_a; eval_y]))
        | x -> x)
    (* eta conversion *)
    | AppView (Lam, [a]) ->
      (match out a with
      | AbsView (x, e) ->
        (match out e with
        | AppView (Ap, [f; xvar]) ->
            (match out xvar with
            | VarView xx -> 
              if (equal (x, xx)) && not (exists (fun y -> equal (xx, y)) (freevars f)) then
                out f
              else
                AppView (Ap, [f; xvar])
            | x -> x)
        | x -> x)
      | x -> x)
    | x -> x
  )

let _ =
  let show = UntypedPrinter.print in

  let x = named "x" in
  let y = named "y" in

  (* \x.x *)
  let id = Lam $$ [x ^^ !! x] in

  (* ((\x.x) y) *)
  let t = Ap $$ [id; !! y] in
  print_string (show t ^ "\n");

  let tt = eval t in
  print_string (show tt ^ "\n");

  let a = named "a" in
  let b = named "b" in

  (* \a.\b.a *)
  let _true = Lam $$ [a ^^ (Lam $$ [b ^^ !! a])] in
  print_string (show _true ^ "\n");

  (* \a.\b.b *)
  let _false = Lam $$ [a ^^ (Lam $$ [b ^^ !! b])] in
  print_string (show _false ^ "\n");

  (* \x.\y.\a.((x y) a *)
  let _if = Lam $$ [x ^^ (Lam $$ [y ^^ (Lam $$ [a ^^ (Ap $$ [(Ap $$ [!! x; !! y]); !! a])])])] in
  print_string ("if: " ^ show _if ^ "\n");

  let if_true_then_true = eval (Ap $$ [(Ap $$ [(Ap $$ [_if; _true]); _true]); _false]) in
  print_string ("if_true_then_true: " ^ show if_true_then_true ^ "\n");    

  (* \a.\b. if a b false *)
  let _and = Lam $$ [a ^^ (Lam $$ [b ^^ (Ap $$ [
                                            (Ap $$ [
                                              (Ap $$ [_if; !! a]
                                              ); !! b]
                                            ); _false]
                                        )
                                  ]
                          )
                    ] in
  print_string (show _and ^ "\n");

  (* eval (and true false) *)
  let and_true_false = eval (Ap $$ [(Ap $$ [_and; _true]); _false]) in
  print_string (show and_true_false ^ "\n");

  let f = named "f" in
  let y_combinator = Lam $$ [f ^^ (Ap $$ [(Lam $$ [x ^^ (Ap $$ [!! f; (Ap $$ [!! x; !! x])])]);
                                          (Lam $$ [x ^^ (Ap $$ [!! f; (Ap $$ [!! x; !! x])])])])] in
  print_string ("y combinator: " ^ show y_combinator ^ "\n");

  let omega = Ap $$ [(Lam $$ [x ^^ (Ap $$ [!! x; !! x])]);
                     (Lam $$ [x ^^ (Ap $$ [!! x; !! x])])] in
  print_string ("omega: " ^ show omega ^ "\n");

  (* Stack overflow *)
  (* eval omega *)

  (* check eta conversion *)
  (* \x.f x *)
  let term = Lam $$ [x ^^ (Ap $$ [!! f; !! x])] in
  print_string (show term ^ "\n");
  print_string (show (eval term) ^ "\n");
