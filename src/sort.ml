module type SORT = sig
  type t
  val eq : t -> t -> bool
  val to_string : t -> string
end
