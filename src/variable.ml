module Var = struct
  module CI = Core_kernel.Core_int
  type t = string * int

  let counter = ref 0

  let newvar s = (s, (counter := !counter + 1; !counter))

  let equal = function ((_ , n), (_, m)) -> n = m

  let compare = function ((_, n), (_, m)) -> CI.compare n m

  let toString (s, n) = s ^ "@" ^ (CI.to_string n)

  let toUserString (s, _) = s
end

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
  val toString : t -> string

  (* Provides the string used to create the variable. *)
  val toUserString : t -> string

end
