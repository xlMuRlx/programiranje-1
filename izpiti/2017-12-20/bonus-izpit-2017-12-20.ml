(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame par in zamenja komponenti para.
   Primer: /obrni (2, 4) = (4, 2)/ *)

let obrni = function
  | (x, y) -> (y, x)


(* 1.2) Definirajte funkcijo, ki vzame par p in vrednost x in zamenja drugo
   komponento para p z x.
   Primer: /zamenjaj_drugo (2, 4) 7 = (2, 7)/ *)

let zamenjaj_drugo par x =
  match par with
  | (y, z) -> (y, x)


(* 1.3) Definirajte funkcijo, ki vzame seznam parov in izračuna nov seznam parov,
   ki imajo drugo komponento zamenjano z 42.
   Primer: /vsem_zamenjaj_drugo_z_42 [(12, 1); (2, 4)] = [(12, 42); (2, 42)]/ *)

let rec vsem_zamenjaj_drugo_z_42 seznam =
  let rec vsi42' acc = function
    | [] -> List.rev acc
    | x :: xs -> vsi42' ((zamenjaj_drugo x 42) :: acc) xs
  in
  vsi42' [] seznam
  

(* 1.4) Definirajte funkcijo, ki varno vrne glavo seznama v primeru, ko seznam ni prazen.
   Uporabite tip option.
   Primer: /glava [1; 2; 3] = Some 1/ *)
  
let glava = function
  | [] -> None
  | x :: xs -> Some x


(* 1.5) Definirajte funkcijo, vzame funkcijo (f: 'a -> 'b), neko vrednost (x : 'a) in
   celo število n. Funkcija naj vrne vrednost, ki jo dobimo če f n-krat uporabimo na x,
   torej f (f ... (f x)...).
   Primer: /uporabi_veckrat succ 0 420 = 420/ *)

let rec uporabi_veckrat f x n =
  match n with
  | 0 -> x
  | k -> uporabi_veckrat f (f x) (k-1)


(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)

type 'a drevo = 
  | Drevo of 'a * 'a drevo list


(* 2.2) Definirajte naslednja rožna drevesa:

   t = 1,  t' =  2   ,      t'' =  3
                / \               /| \
               t  t              / |  \
                               -1  t'  0

 *)

let t = Drevo(1, [])
let t' = Drevo(2, [t; t])
let t'' = Drevo(3, [Drevo(-1, []); t'; Drevo(0, [])])


(* 2.3) Definirajte funkcijo, ki vrne gozd rožnega drevesa. *)

let rec vrni_gozd = function
  | Drevo(_, xs) -> xs


(* 2.4) Definirajte funkcijo, ki izpiše vse vrednosti v rožnem drevesu celih števil.
   Števila naj bodo v ločenih vrsticah. Uporabite (print_int : int -> unit) in
   (print_newline : unit -> unit). *)

let rec izpisi_vrednosti drevo =
  let rec izpisi_seznam = function
    | [] -> ()
    | x :: xs -> (
      match x with
      | Drevo(y, ys) -> (
        print_int y;
        print_newline();
        izpisi_seznam ys
      )
    )
  in
  match drevo with
  | Drevo(x, xs) -> (
    print_int x;
    print_newline();
    izpisi_seznam xs
  )



(* 2.5) Definirajte funkcijo, ki izračuna globino rožnega drevesa, t.j. dolžino
   najdaljše poti od korena do lista. *)

let rec max_seznama seznam =
  if seznam = [] then
    failwith "Seznam je prekratek!"
  else
    let rec max' acc = function
      | [] -> acc
      | x :: xs -> 
        if x > acc then
          max' x xs
        else
          max' acc xs
    in
    max' (List.hd seznam) seznam


let rec globina drevo =
  let rec globina' acc = function
  | Drevo (_, []) -> 1 + max_seznama acc
  | Drevo (_, xs) -> (
    match xs with
    | [] -> failwith "Se ne zgodi!"
    | y :: ys -> globina' y
  )

(* 2.6) Definirajte funkcijo, ki sestavi (poljubno) rožno drevo globine n.
   Vrednosti v korenih so poljubne. *)
let globoko_drevo = failwith "dopolni me"

(* 2.7) Definirajte funkcijo, ki sprejme funkcijo (f : 'b -> 'a -> 'b) in začetno vrednost (acc : 'b)
   in funkcijo f zloži [fold] preko drevesa (t : 'a drevo). Vrstni red pri tem ni pomemben.

   Za primer t' želimo vrednost f (f (f acc 1) 2) 2)  (lahko tudi f (f (f acc 2) 1) 2))
   Primer: /zlozi (fun acc x -> x + acc) 0 t'' = 6/

   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)
let zlozi = failwith "dopolni me"
