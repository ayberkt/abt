module type VARIABLE =
sig
  type t

  (* Creates a new, globally unique variable. *)
  val newvar : string -> t

  (* Tests whether two variable are equal. *)
  val equal : t * t -> bool

  (* Compares two variables.
     This is used to allow variables as keys into a hash table. *)
  val compare : t * t -> int

  (* Provides a string representation of the globally unique
     variable. *)
  val to_string : t -> string

  (* Provides the string used to create the variable. *)
  val toUserString : t -> string
end

module Var : (VARIABLE with type t = (string * int)) = struct
  module CI = Core_kernel.Core_int
  type t = string * int

  let counter = ref 0

  let newvar s =
    let _ = counter := !counter + 1 in
    (s, !counter)

  let equal = function ((_ , n), (_, m)) -> n = m

  let compare = function ((_, n), (_, m)) -> CI.compare n m

  let to_string (s, n) = s ^ "@" ^ (CI.to_string n)

  let toUserString (s, _) = s
end
