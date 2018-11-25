(* ========== Vaja 2: Funkcijsko Programiranje  ========== *)

(* #use "5-funkcijsko-programiranje/vaje/funkcije.ml" *)

(*----------------------------------------------------------------------------*]
Namig: Definirajte pomožno funkcijo za obračanje seznamov.
[*----------------------------------------------------------------------------*)


let rec reverse_slabo = function
  | [] -> []
  | x :: xs -> reverse_slabo xs @ [x]


let rec reverse sez =
  let rec reverse' acc = function
    | [] -> acc
    | glava :: rep -> reverse' (glava :: acc) rep
    in
    reverse' [] sez


(*----------------------------------------------------------------------------*]
 Funkcija [repeat x n] vrne seznam [n] ponovitev vrednosti [x]. Za neprimerne
 vrednosti [n] funkcija vrne prazen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
[*----------------------------------------------------------------------------*)


let rec repeat x n =
  if n <= 0 then
   []
  else
    x :: repeat x (n - 1)


let rec repeat_boljse x n =
  let rec repeat' x n acc =
    if n <= 0 then
      acc
    else
      let new_acc = x :: acc in
      repeat' x (n-1) new_acc
  in
  repeat' x n []


(*----------------------------------------------------------------------------*]
 Funkcija [range] sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
[*----------------------------------------------------------------------------*)


let rec range n = 
  if n < 0 then
    []
  else
    (range (n - 1)) @ [n]


let rec range_boljse n =
  let rec range' n acc =
    if n < 0 then
      acc
    else
      range' (n - 1) (n :: acc)
  in
  range' n []


(*----------------------------------------------------------------------------*]
 Funkcija [map f list] sprejme seznam [list] oblike [x0; x1; x2; ...] in
 funkcijo [f] ter vrne seznam preslikanih vrednosti, torej
 [f(x0); f(x1); f(x2); ...].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (+)2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)


let rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs


(*----------------------------------------------------------------------------*]
 Funkcija [map_tlrec] je repno rekurzivna različica funkcije [map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (fun x -> x+2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)


let rec map_tlrec f list = 
  let rec map_tlrec' acc = function
    | [] -> reverse acc
    | x :: xs -> map_tlrec' (f x :: acc) xs
  in
  map_tlrec' [] list


(*----------------------------------------------------------------------------*]
 Funkcija [mapi] sprejme seznam in funkcijo dveh argumentov ter vrne seznam
 preslikanih vrednosti seznama, kjer kot drugi argument funkcije podamo indeks
 elementa v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
[*----------------------------------------------------------------------------*)


let rec mapi f list =
  let rec mapi' f i = function
      | [] -> []
      | x :: xs -> (f x i) :: (mapi' f (i+1) xs)
  in 
  mapi' f 0 list


(*----------------------------------------------------------------------------*]
 Funkcija [zip] sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Different lengths of input lists.".
[*----------------------------------------------------------------------------*)


let rec zip list1 list2 = 
  match (list1, list2) with
  | [], [] -> []
  | _::_, [] -> failwith "Different lengths of input lists."
  | [], _::_ -> failwith "Different lengths of input lists."
  | x :: xs, y :: ys -> (x, y) :: zip xs ys


(*----------------------------------------------------------------------------*]
 Funkcija [zip_enum_tlrec] sprejme seznama [x_0; x_1; ...] in [y_0; y_1; ...]
 ter vrne seznam [(0, x_0, y_0); (1, x_1, y_1); ...]. Funkcija je repno
 rekurzivna. Če seznama nista enake dolžine vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip_enum_tlrec ["a"; "b"; "c"] [7; 3; 4];;
 - : (int * string * int) list = [(0, "a", 7); (1, "b", 3); (2, "c", 4)]
[*----------------------------------------------------------------------------*)


let rec zip_enum_tlrec list1 list2 = 
  let rec zip_enum_tlrec' list1 list2 k acc =
    match list1, list2 with
    | [], [] -> reverse acc
    | _, [] -> failwith "Different lengths of input lists."
    | [], _ -> failwith "Different lengths of input lists."
    | x :: xs, y :: ys -> zip_enum_tlrec' xs ys (k+1) ((k, x, y) :: acc)
  in
  zip_enum_tlrec' list1 list2 0 []


(*----------------------------------------------------------------------------*]
 Funkcija [unzip] je inverz funkcije [zip], torej sprejme seznam parov
 [(x0, y0); (x1, y1); ...] in vrne par seznamov ([x0; x1; ...], [y0; y1; ...]).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)


let rec unzip = function
  | [] -> ([], [])
  | (x, y) :: xs -> let (list1, list2) = unzip xs in (x :: list1, y :: list2)


(*----------------------------------------------------------------------------*]
 Funkcija [unzip_tlrec] je repno rekurzivna različica funkcije [unzip].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)


let rec unzip_tlrec seznam =
  let rec unzip' acc1 acc2 = function
    | [] -> (reverse acc1, reverse acc2)
    | (x, y) :: xs -> unzip' (x :: acc1) (y :: acc2) xs
  in
  unzip' [] [] seznam


(*----------------------------------------------------------------------------*]
 Funkcija [fold_left_no_acc f list] sprejme seznam [x0; x1; ...; xn] in
 funkcijo dveh argumentov [f] in vrne vrednost izračuna
 f(... (f (f x0 x1) x2) ... xn).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
 - : string = "FICUS"
[*----------------------------------------------------------------------------*)


let rec fold_left_no_acc f = function
  | [] | _ :: [] -> failwith "Seznam je prekratek!"
  | x :: y :: [] -> f x y
  | x :: y :: xs -> fold_left_no_acc f ((f x y) :: xs)


(*----------------------------------------------------------------------------*]
 Funkcija [apply_sequence f x n] vrne seznam zaporednih uporab funkcije [f] na
 vrednosti [x] do vključno [n]-te uporabe, torej
 [x; f x; f (f x); ...; (f uporabljena n-krat na x)].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # apply_sequence (fun x -> x * x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x * x) 2 (-5);;
 - : int list = []
[*----------------------------------------------------------------------------*)


let rec apply_sequence f x n =
  let rec apply' f x n acc =
    if n <= 0 then reverse acc
    else apply' f (f x) (n-1) ((f x) :: acc)
  in
  apply' f x n []


(*----------------------------------------------------------------------------*]
 Funkcija [filter f list] vrne seznam elementov [list], pri katerih funkcija [f]
 vrne vrednost [true].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
[*----------------------------------------------------------------------------*)


let rec filter f seznam =
  let rec filter' f acc = function
    | [] -> reverse acc
    | x :: xs -> if f x then 
                    filter' f (x :: acc) xs
                  else
                    filter' f acc xs
  in
  filter' f [] seznam


(*----------------------------------------------------------------------------*]
 Funkcija [exists] sprejme seznam in funkcijo, ter vrne vrednost [true] čim
 obstaja element seznama, za katerega funkcija vrne [true] in [false] sicer.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # exists ((<)3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<)8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
[*----------------------------------------------------------------------------*)


let rec exists f seznam =
  filter f seznam <> []


(*----------------------------------------------------------------------------*]
 Funkcija [first f default list] vrne prvi element seznama, za katerega
 funkcija [f] vrne [true]. Če takšnega elementa ni, vrne [default].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # first ((<)3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<)8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
[*----------------------------------------------------------------------------*)


let rec prvi_element = function
  | [] -> failwith "Seznam je prekratek!"
  | x :: xs -> x


let rec first f default seznam =
  if exists f seznam then 
    let seznam_veljavnih = filter f seznam
    in prvi_element seznam_veljavnih
  else
    default