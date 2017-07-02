module type VARIABLE =
sig
  type t

  (* Creates a new, named globally unique variable. *)
  val named : string -> t

  (* Creates a new, globally unique variable. *)
  val newvar : unit -> t

  (* Tests whether two variable are equal. *)
  val equal : t * t -> bool

  (* Compares two variables.
     This is used to allow variables as keys into a hash table. *)
  val compare : t * t -> int

  (* Provides a string representation of the globally unique
     variable. *)
  val to_string : t -> string

  (* Clones a variable, to save its name *)
  val clone : t -> t

  (* Provides the string used to create the variable. *)
  val toUserString : t -> string
end

module Var : (VARIABLE with type t = (string option * int)) = struct
  module CI = Core_kernel.Core_int
  type t = string option * int

  let counter = ref 0

  let named s =
    let _ = counter := !counter + 1 in
    (Some s, !counter)

  let newvar () =
    let _ = counter := !counter + 1 in
    (None, !counter)

  let compare ((s, n), (s', m)) =
    (match ((s, s'), CI.compare n m) with
    | ((Some s, Some s'), 0) -> String.compare s s'
    | (_, order) -> order)

  let equal (x, y) = compare (x, y) = 0

  let to_string (s, n) =
    let str = (match s with Some s -> s | None -> "") in
    str ^ "@" ^ (CI.to_string n)

  let clone (s, _) =
    match s with
    | Some s -> named s
    | _ -> newvar ()

  let toUserString (s, _) =
    match s with
      Some s -> s
    | None -> ""
end
