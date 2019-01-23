(* ===== PRVA NALOGA ===== *)

(* a *)
let uporabi f x =
  f x

(* b *)
let ibaropu x f =
  f x

(* c *)
let rec zacetnih n seznam =
  let rec zacetnih' n acc seznam =
    match n, seznam with
    | 0, _ -> Some (List.rev acc)
    | _, [] -> None
    | k, x :: xs -> zacetnih' (k - 1) (x :: acc) xs
  in
  zacetnih' n [] seznam




(* ===== DRUGA NALOGA ===== *)

type 'a neprazen_sez =
  | Konec of 'a
  | Sestavljen of 'a * 'a neprazen_sez

let test = Sestavljen(3, Sestavljen(5, Sestavljen(8, Sestavljen(13, Konec(2)))))


(* a *)
let rec prvi = function
  | Konec(x) -> x
  | Sestavljen(x, xs) -> x

let rec zadnji = function
  | Konec(x) -> x
  | Sestavljen(_, xs) -> zadnji xs


(* b *)
let rec dolzina = function
  | Konec (_) -> 1
  | Sestavljen(_, xs) -> 1 + dolzina xs


(* c *)
let rec pretvori_v_seznam = function
  | Konec(x) -> [x]
  | Sestavljen(x, xs) -> x :: pretvori_v_seznam xs


(* d *)
let rec zlozi f = function
  | Konec(x) -> Konec(f x)
  | Sestavljen(x, xs) -> Sestavljen(f x, zlozi f xs)





(* ===== TRETJA NALOGA ===== *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;


(* a *)
let rec simetricen niz =
  explode niz = List.rev(explode niz)


(* b *)