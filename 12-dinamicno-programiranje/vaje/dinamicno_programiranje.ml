(* ========== Vaje 6: Dinamično programiranje  ========== *)

let memoiziraj f =
  let rezultati = Hashtbl.create 512 in
  let mem_f x =
    match Hashtbl.find_opt rezultati x with
    | None ->
        let y = f x in
        Hashtbl.add rezultati x y;
        y
    | Some y ->
        y
  in
  mem_f


(* Za starejše verzije OCamla: *)
let memoiziraj_rec odviti_f =
  let rezultati = Hashtbl.create 512 in
  let rec mem_f x =
    if Hashtbl.mem rezultati x then
      Hashtbl.find rezultati x
    else
      let y = odviti_f mem_f x in
      Hashtbl.add rezultati x y;
      y
  in
  mem_f



(*----------------------------------------------------------------------------*]
 Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
 samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
 v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
 različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
 zanima, katero pot naj ubere.

 Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
 sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
 optimalni poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_cheese test_matrix;;
 - : int = 13
[*----------------------------------------------------------------------------*)

let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]


let max_cheese cheese_matrix =
  let max_vrstica = Array.length cheese_matrix in
  let max_stolpec = Array.length cheese_matrix.(0) in
  (* To lahko uporabimo, ker pričakujemo enako dolžino vseh vrstic *)

  let max_cheese' recursive_max_cheese' (vrstica, stolpec) =
    (* Indeksa sta neprimerna! *)
    if vrstica >= max_vrstica || stolpec >= max_stolpec then
      0 (* Ker s tem dosežemo, da se na rešitev ne vpliva, pogoj pa velja tudi za 1x1 matriko *)
      (* Lahko bi imeli tudi več zaustavitevenih pogojev, npr. število korakov *)
    else
      let desno = recursive_max_cheese' (vrstica, stolpec + 1) in
      (* S premikom navzdol se vrstica za 1 poveča *)
      let spodaj = recursive_max_cheese' (vrstica + 1, stolpec) in
      (* S premikom desno se stolpec za 1 poveča *)
      let our_cheese = cheese_matrix.(vrstica).(stolpec) in
      our_cheese + max desno spodaj
  in
  let memoised_max_cheese = memoiziraj_rec max_cheese' in
  memoised_max_cheese (0, 0)


(* Funkcija deluje hitreje s pomočjo memoizacije, saj si vmesne rezultate zapomni in 
jih zato ni potrebno računati večkrat. *)


(*----------------------------------------------------------------------------*]
 Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
 različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
 3, rdeči pa višin 1 in 2.

 Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
 dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
 v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
 poljubne barve.

 Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # alternating_towers 10;;
 - : int = 35
[*----------------------------------------------------------------------------*)

let rec rdeci = function
  | 0 -> 1
  | n when n < 0 -> 0
  | n -> modri (n - 1) +  modri (n - 2)

and modri = function
  | 0 -> 1
  | n when n < 0 -> 0
  | n -> rdeci (n - 2) + rdeci (n - 3)


let alternating_towers = function
  | 0 -> 1
  | n -> rdeci n + modri n

(*----------------------------------------------------------------------------*]
 Na nagradni igri ste zadeli kupon, ki vam omogoča, da v Mercatorju kupite
 poljubne izdelke, katerih skupna masa ne presega [max_w] kilogramov. Napišite
 funkcijo [best_value articles max_w], ki poišče največjo skupno ceno, ki jo
 lahko odnesemo iz trgovine, kjer lahko vsak izdelek vzamemo večkrat, nato pa
 še funkcijo [best_value_uniques articles max_w], kjer lahko vsak izdelek
 vzamemo kvečjemu enkrat.
 Namig: Modul [Array] ponuja funkcije kot so [map], [fold_left], [copy] in
 podobno, kot alternativa uporabi zank.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # best_value articles 1.;;
 - : float = 10.95
 # best_value_unique articles 1.;;
- : float = 7.66
[*----------------------------------------------------------------------------*)

(* Articles are of form (name, price, weight) *)
let articles = [|
	("yoghurt", 0.39, 0.18);
	("milk", 0.89, 1.03);
  ("coffee", 2.19, 0.2);
  ("butter", 1.49, 0.25);
  ("yeast", 0.22, 0.042);
  ("eggs", 2.39, 0.69);
  ("sausage", 3.76, 0.50);
  ("bread", 2.99, 1.0);
  ("Nutella", 4.99, 0.75);
  ("juice", 1.15, 2.0)
|]


let best_value articles max_w =
  let rec get_item acc_w acc_p (_, p, w) =
    if acc_w +. w > max_w then
      acc_p
    else
      shopper (acc_w +. w) (acc_p +. p)
  and shopper w p =
    let choices = Array.map (get_item w p) articles in
    Array.fold_left max 0. choices
  in
  shopper 0. 0.