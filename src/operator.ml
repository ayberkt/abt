module type OPERATOR = sig
  module St : Sort.SORT
  type op

  type valence = St.t list * St.t
  type arity   = valence list * St.t

  val ar : op -> arity
  val equal : op * op -> bool
  val to_string : op -> string
end
