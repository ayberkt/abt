module Util = struct
  let mem equal x xs = List.exists (fun y -> equal(x, y)) xs
  let remove equal x xs = List.filter (fun y -> not (equal(x,y))) xs

  let rec zip_exact (e : exn) xs ys =
    match (xs, ys) with
    | [], [] -> []
    | (x::xs), (y::ys) -> (x,y) :: (zip_exact e xs ys)
    | _ -> raise e

  (* Quadratic(!) algorithm to make a list without duplicates *)

  let collate equal xss =
    let rec loop acc xs =
      match xs with
      | [] -> []
      | ([] :: yss) -> loop acc yss
      | ((y :: ys) :: yss) when mem equal y acc -> loop acc (ys :: yss)
      | ((y :: ys) :: yss) -> loop (y :: acc) (ys :: yss)
    in
        loop [] xss

  let rec zipTest p xs ys =
    match (xs, ys) with
    | [], [] -> true
    | (x::xs), (y::ys) -> p(x, y) && zipTest p xs ys
    | _  -> false

end
