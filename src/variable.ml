open Sort

module type VARIABLE =
sig
  type sort
  type t

  (* Creates a new, named globally unique variable. *)
  val named : sort -> string -> t

  (* Creates a new, globally unique variable. *)
  val newvar : sort -> unit -> t

  val vsort : t -> sort

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

module MakeVar (S : SORT) : VARIABLE with type sort = S.t = struct
  module CI = Core_kernel.Core_int
  type sort = S.t
  type t = sort * string * int

  let counter = ref 0

  let named srt s =
    let _ = counter := !counter + 1 in
    (srt, Some s, !counter)

  let newvar srt () =
    let _ = counter := !counter + 1 in
    (srt, None, !counter)

  let compare ((_, s, n), (_, s', m)) =
    (match ((s, s'), BI.compare n m) with
    | ((Some s, Some s'), 0) -> String.compare s s'
    | (_, order) -> order)

  let equal (x, y) = compare (x, y) = 0

  let to_string (_, s, n) =
    let str = (match s with Some s -> s | None -> "") in
    str ^ "@" ^ (BI.to_string n)

  let clone (s, _) =
    match s with
    | Some s -> named s
    | _ -> newvar ()

  let toUserString (_, s, _) =
    match s with
      Some s -> s
    | None -> ""

  let vsort (s, _, _) = s
end
