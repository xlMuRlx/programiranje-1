
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


let max_cheese matrika =
  let max_vrstica = Array.length matrika in
  let max_stolpec = Array.length matrika.(0) in
  let rec max_cheese' rec_max_cheese' (vrstica, stolpec) =
    if vrstica >= max_vrstica || stolpec >= max_stolpec then 
      0
    else
      let spodaj = rec_max_cheese' (vrstica + 1, stolpec) in
      let desno = rec_max_cheese' (vrstica, stolpec + 1) in
      let sir = matrika.(vrstica).(stolpec) in
      sir + max spodaj desno
  in
  let memoised_max_cheese = memoiziraj_rec max_cheese' in
  memoised_max_cheese (0, 0)