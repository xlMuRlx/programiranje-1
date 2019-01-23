(* ===== PRVA NALOGA ===== *)


(* a *)
let rec izpisi_vsa_stevila = function
  | [] -> ()
  | x :: xs -> (
    print_int(x);
    izpisi_vsa_stevila xs
  )


(* b *)
let rec map2_opt f sez1 sez2 =
  let rec map2' f sez1 sez2 acc =
    match sez1, sez2 with
    | [], [] -> Some(List.rev acc)
    | _, [] -> None
    | [], _ -> None
    | x :: xs, y :: ys -> map2' f xs ys ((f x y) :: acc)
  in
  map2' f sez1 sez2 []





(* ===== DRUGA NALOGA ===== *)

(* a *)
type filter_tree =
  | Vozlisce of filter_tree * int * filter_tree
  | List of int list

let primer =
  let levo = Vozlisce(List([1]), 5, List([])) in
  let desno = Vozlisce(List([]), 15, List([19; 20])) in
  Vozlisce(levo, 10, desno)


(* b *)
let rec vstavi n = function
  | List(xs) -> List(n :: xs)
  | Vozlisce(levo, x, desno) ->
    if n <= x then
      Vozlisce(vstavi n levo, x, desno)
    else
      Vozlisce(levo, x, vstavi n desno)


(* c *)
let rec vstavi_seznam seznam drevo =
  match seznam with
  | [] -> drevo
  | x :: xs -> vstavi_seznam xs (vstavi x drevo)


(* d *)
let rec preveri_stevila drevo =
  let rec izpisi_stevila = function
    | List(xs) -> xs
    | Vozlisce(levo, x, desno) -> (izpisi_stevila levo) @ (izpisi_stevila desno)
  in
  let rec izprazni = function
    | List(_) -> List([])
    | Vozlisce(levi, x, desni) -> Vozlisce(izprazni levi, x, izprazni desni)
  in
  drevo = vstavi_seznam (List.rev (izpisi_stevila drevo)) (izprazni drevo)




(* ===== TRETJA NALOGA ===== *)

type vektor = int * int
type matrika = int * int * int * int

module type Linearna = sig
  type t

  val id : t
  val uporabi : t -> vektor -> vektor
  val iz_matrike : matrika -> t
  val iz_funkcije : (vektor -> vektor) -> t
  val kompozitum : t -> t -> t
end



(* a *)

module Matrika : Linearna = struct
  type t = int * int * int * int

  let id = (1, 0, 0, 1)

  let uporabi mat vec = 
    match mat, vec with
    | (a, b, c, d), (x, y) -> (a*x + b*y, c*x + d*y)

  let iz_matrike mat = mat

  let iz_funkcije f =
    let (a, c) = f (1, 0) in
    let (b, d) = f (0, 1) in
    (a, b, c, d)

  let kompozitum mat1 mat2 =
    match mat1, mat2 with
    | (a, b, c, d), (e, f, g, h) -> (a*e + b*g, a*f + b*h, c*e + d*g, d*f + d*h)
end


module Funkcija : Linearna = struct
  type t = int * int -> int * int

  let id = (fun x -> x)

  let uporabi v f = f v

  let iz_matrike (a, b, c, d) = fun (x, y) -> (x*a + y*b, x*c + y*d)

  let iz_funkcije f = f
  
  let kompozitum f g = fun x -> f (g x)
end