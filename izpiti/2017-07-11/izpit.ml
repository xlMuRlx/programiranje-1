(* ===== PRVA NALOGA ===== *)

type rezultati = 
  | Neudelezen
  | Rezultat of int * int * int * int

type student = 
  | Student of string * string * int * rezultati

let a = Student("Ana", "Bertoncelj", 1, Rezultat(10, 20, 15, 2))
let b = Student("Bine", "Cencelj", 2, Rezultat(5, 13, 20, 17))
let c = Student("Cilka", "Drnovsek", 3, Neudelezen)

let l = [a; b; c]


(* a *)
let rec vsota_tock = function
  | Student(_, _, _, rezultat) -> (
    match rezultat with
    | Neudelezen -> 0
    | Rezultat(x, y, z, w) -> x + y + z+ w
  )


(* b *)
let rec najboljsi letnik =
  let rec najboljsi' trenutni = function
    | [] -> trenutni
    | x :: xs -> (
      match vsota_tock x with
      | y -> 
        if y >= vsota_tock trenutni then
          najboljsi' x xs
        else
          najboljsi' trenutni xs
    )
  in
  najboljsi' (Student("nicla", "nicla", 0, Rezultat(0, 0, 0, 0))) letnik





(* ===== DRUGA NALOGA ===== *)

type vozlisca =
  | Vozlisca of int list

type povezave =
  | Povezave of (int * int) list

let prazni = (Vozlisca([]), Povezave([]))

let g = 
  let vozlisca = Vozlisca([1; 2; 3; 4; 5]) in
  let povezave = Povezave([(1,2); (1,3); (2,4); (3,5); (4,5)]) in
  (vozlisca, povezave)






(* ===== ČETRTA NALOGA ===== *)

