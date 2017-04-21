module type OPERATOR = sig
  type t
  val arity : t -> int list
  val equal : t * t -> bool
  val to_string : t -> string
end
