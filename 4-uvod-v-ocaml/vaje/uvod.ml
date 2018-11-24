
(* ========== Vaja 1: Uvod v OCaml  ========== *)

(*Začasno: za zagon datoteke -> #use "4-uvod-v-ocaml/vaje/uvod.ml" *)

(*----------------------------------------------------------------------------*]
 Funkcija [penultimate_element] vrne predzadnji element danega seznama. V
 primeru prekratkega seznama vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # penultimate_element [1; 2; 3; 4];;
 - : int = 3
[*----------------------------------------------------------------------------*)


let rec ultimate_element list =
  match list with
  | [] -> failwith "Seznam je prekratek!"
  | x :: [] -> x
  | _ :: ys -> ultimate_element ys
(* To je primer funkcije, ki vrne zadnji element seznama. *)


(* Krajši in boljši zapis funkcije (ker ne definiramo nepotrebnih argumentov). *)
let rec penultimate_element = function
  | [] | _ :: [] -> failwith "Seznam je prekratek!"
  | x :: _ :: [] -> x
  | _ :: y :: ys -> penultimate_element (y :: ys)


(*----------------------------------------------------------------------------*]
 Funkcija [get k list] poišče [k]-ti element v seznamu [list]. Številčenje
 elementov seznama (kot ponavadi) pričnemo z 0. Če je k negativen, funkcija
 vrne ničti element. V primeru prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)


(* Uporabimo match, ker uporabljamo 2 argumenta. *)
let rec get1 k list =
  match (k, list) with
  | _, [] -> failwith "Seznam je prekratek!"
  | k, x :: xs when k <= 0 -> x
  | k, x :: xs -> get1 (k - 1) xs


(* Ker se k dejansko ne uporablja v dejanski funkciji (nimamo match na njem): *)
let rec get k = function
  | [] -> failwith "Seznam je prekratek!"
  | x :: xs when (k <= 0) -> x
  | x :: xs -> get (k - 1) xs


(* Funkcija, ki vrne prvi element. *)
let get_first = get 1

(*----------------------------------------------------------------------------*]
 Funkcija [double] podvoji pojavitve elementov v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # podvoji [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)


let rec double = function
  | [] -> []
  | x :: xs -> x :: x :: double xs


(*----------------------------------------------------------------------------*]
 Funkcija [divide k list] seznam razdeli na dva seznama. Prvi vsebuje prvih [k]
 elementov, drugi pa vse ostale. Funkcija vrne par teh seznamov. V primeru, ko
 je [k] izven mej seznama, je primeren od seznamov prazen.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)


let rec divide k list =
  match k, list with
  | k, list when (k <= 0 ) -> ([], list)
  | k, [] -> ([], [])
  | k, x :: xs ->
  let (left_list, right_list) = divide (k-1) xs in
  (x :: left_list, right_list)


(*----------------------------------------------------------------------------*]
 Funkcija [delete k list] iz seznama izbriše [k]-ti element. V primeru
 prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # delete 3 [0; 0; 0; 1; 0; 0];;
 - : int list = [0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)


let rec delete k list1 = 
  match k, list1 with
  | _, [] -> failwith "Seznam je prekratek!"
  | 0, x :: xs -> xs
  | k, x :: xs -> x :: delete (k-1) xs

  
(*----------------------------------------------------------------------------*]
 Funkcija [slice i k list] sestavi nov seznam, ki vsebuje elemente seznama
 [list] od vključno [i]-tega do izključno [k]-tega. Predpostavimo, da sta [i] in
 [k] primerna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # slice 3 6 [0; 0; 0; 1; 2; 3; 0; 0];;
 - : int list = [1; 2; 3]
[*----------------------------------------------------------------------------*)


let slice i k seznam =
  let (_, slice1) = divide i seznam in
  let (slice2, _) = divide (k - i) slice1 in
  slice2


(*----------------------------------------------------------------------------*]
 Funkcija [insert x k list] na [k]-to mesto seznama [list] vrine element [x].
 Če je [k] izven mej seznama, ga funkcija doda na začetek oziroma na konec.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)


let rec insert x k seznam = 
  match k, seznam with
  | _, [] -> [x]
  | 0, sez -> x :: sez
  | k, y :: ys -> y :: insert x (k-1) ys


(*----------------------------------------------------------------------------*]
 Funkcija [rotate n list] seznam zavrti za [n] mest v levo. Predpostavimo, da
 je [n] v mejah seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)


let rec rotate n seznam =
  let rec rotate' n seznam acc =
    match n, seznam with
    | _, [] -> []
    | 0, sez -> sez @ acc
    | n, x :: xs -> rotate' (n-1) xs (acc @ [x])
  in
  rotate' n seznam []


(*----------------------------------------------------------------------------*]
 Funkcija [remove x list] iz seznama izbriše vse pojavitve elementa [x].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)


let rec remove x = function
    | [] -> []
    | y :: ys -> if y = x then remove x ys else y :: remove x ys


(*----------------------------------------------------------------------------*]
 Funkcija [is_palindrome] za dani seznam ugotovi ali predstavlja palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)


let rec is_palindrome seznam = 
  let rec obrni = function
    | [] -> []
    | x :: xs -> (obrni xs) @ [x]
  in
  seznam = obrni seznam


(*----------------------------------------------------------------------------*]
 Funkcija [max_on_components] sprejme dva seznama in vrne nov seznam, katerega
 elementi so večji od istoležnih elementov na danih seznamih. Skupni seznam ima
 dolžino krajšega od danih seznamov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)


let rec max_on_components seznam1 seznam2 =
  match seznam1, seznam2 with
    | [], _ -> []
    | _, [] -> []
    | x :: xs, y :: ys -> if x > y then x :: max_on_components xs ys else y :: max_on_components xs ys


(*----------------------------------------------------------------------------*]
 Funkcija [second_largest] vrne drugo največjo vrednost v seznamu. Pri tem se
 ponovitve elementa štejejo kot ena vrednost. Predpostavimo, da ima seznam vsaj
 dve različni vrednosti.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)


let rec second_largest seznam =
  let rec largest = function
    | [] -> failwith "Seznam je prekratek!"
    | x :: [] -> x
    | x :: xs -> max x (largest xs)
  in
  largest (remove (largest seznam) seznam)

