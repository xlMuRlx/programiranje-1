(* ========== Vaje 5: Urejanje  ========== *)


(*----------------------------------------------------------------------------*]
 Funkcija [randlist len max] generira seznam dolžine [len] z naključnimi
 celimi števili med 0 in [max].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let l = randlist 10 10 ;;
 val l : int list = [0; 1; 0; 4; 0; 9; 1; 2; 5; 4]
[*----------------------------------------------------------------------------*)

let rec randlist len max =
  let rec randlist' max acc = function
    | 0 -> acc
    | k -> randlist' max ((Random.int max) :: acc) (k - 1)
  in
  randlist' max [] len

let l = randlist 10 10 ;;


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Sedaj lahko s pomočjo [randlist] primerjamo našo urejevalno funkcijo (imenovana
 [our_sort] v spodnjem primeru) z urejevalno funkcijo modula [List]. Prav tako
 lahko na manjšem seznamu preverimo v čem je problem.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 let test = (randlist 100 100) in (our_sort test = List.sort compare test);;
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

let test_sort_fun our_sort =
  let test = (randlist 100 100) in 
  (our_sort test = List.sort compare test)


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Vstavljanjem
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert y xs] vstavi [y] v že urejen seznam [xs] in vrne urejen
 seznam. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 9 [0; 2];;
 - : int list = [0; 2; 9]
 # insert 1 [4; 5];;
 - : int list = [1; 4; 5]
 # insert 7 [];;
 - : int list = [7]
[*----------------------------------------------------------------------------*)

let rec insert y = function
  | [] -> [y]
  | x :: xs -> if y <= x then y :: x :: xs else x :: (insert y xs)


(*----------------------------------------------------------------------------*]
 Prazen seznam je že urejen. Funkcija [insert_sort] uredi seznam tako da
 zaporedoma vstavlja vse elemente seznama v prazen seznam.
[*----------------------------------------------------------------------------*)

let rec insert_sort seznam =
  let rec insert_sort' acc = function
    | [] -> acc
    | x :: xs -> insert_sort' (insert x acc) xs
  in
  insert_sort' [] seznam


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Izbiranjem
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*----------------------------------------------------------------------------*]
 Funkcija [min_and_rest list] vrne par [Some (z, list')] tako da je [z]
 najmanjši element v [list] in seznam [list'] enak [list] z odstranjeno prvo
 pojavitvijo elementa [z]. V primeru praznega seznama vrne [None]. 
[*----------------------------------------------------------------------------*)

let rec prvi_element = function
  | [] -> failwith "Seznam je prekratek!"
  | x :: xs -> x

let rec min_seznam seznam = 
  let rec min_seznam' k = function
    | [] -> k
    | x :: xs -> if x <= k then min_seznam' x xs else min_seznam' k xs
  in
  min_seznam' (prvi_element seznam) seznam


let rec min_and_rest seznam =
  if seznam = [] then None
  else
    let k = min_seznam seznam
    in
    let rec rest k seznam =
      match seznam with
      | [] -> []
      | x :: xs -> if x = k then xs else  x :: (rest k xs)
    in
    Some (k, rest k seznam)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri urejanju z izbiranjem na vsakem koraku ločimo dva podseznama, kjer je prvi
 že urejen, drugi pa vsebuje vse elemente, ki jih je še potrebno urediti. Nato
 zaporedoma prenašamo najmanjši element neurejenega podseznama v urejen
 podseznam, dokler ne uredimo vseh. 

 Če pričnemo z praznim urejenim podseznamom, vemo, da so na vsakem koraku vsi
 elementi neurejenega podseznama večji ali enaki elementom urejenega podseznama,
 saj vedno prenesemo najmanjšega. Tako vemo, da moramo naslednji najmanjši člen
 dodati na konec urejenega podseznama.
 (Hitreje je obrniti vrstni red seznama kot na vsakem koraku uporabiti [@].)
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

let rec obrni sez =
  let rec reverse' acc = function
    | [] -> acc
    | glava :: rep -> reverse' (glava :: acc) rep
    in
    reverse' [] sez

(*----------------------------------------------------------------------------*]
 Funkcija [selection_sort] je implementacija zgoraj opisanega algoritma.
 Namig: Uporabi [min_and_rest] iz prejšnje naloge.
[*----------------------------------------------------------------------------*)

let rec selection_sort seznam =
  let rec sort' acc = function
    | [] -> obrni acc
    | sez -> (
      match min_and_rest sez with
      | None -> failwith "Se ne zgodi!"
      | Some (x, xs) -> sort' (x :: acc) xs
    )
  in
  sort' [] seznam


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Izbiranjem na Tabelah
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri delu z tabelami (array) namesto seznami, lahko urejanje z izbiranjem 
 naredimo "na mestu", t.j. brez uporabe vmesnih kopij (delov) vhoda. Kot prej
 tabelo ločujemo na že urejen del in še neurejen del, le da tokrat vse elemente
 hranimo v vhodni tabeli, mejo med deloma pa hranimo v spremenljivki
 [boundary_sorted]. Na vsakem koraku tako ne izvlečemo najmanjšega elementa
 neurejenga dela tabele temveč poiščemo njegov indeks in ga zamenjamo z
 elementom na meji med deloma (in s tem dodamo na konec urejenega dela).
 Postopek končamo, ko meja doseže konec tabele.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [swap a i j] zamenja elementa [a.(i)] and [a.(j)]. Zamenjavo naredi
 na mestu in vrne unit.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let test = [|0; 1; 2; 3; 4|];;
 val test : int array = [|0; 1; 2; 3; 4|]
 # swap test 1 4;;
 - : unit = ()
 # test;;
 - : int array = [|0; 4; 2; 3; 1|]
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 Funkcija [index_min a lower upper] poišče indeks najmanjšega elementa tabele
 [a] med indeksoma [lower] and [upper] (oba indeksa sta vključena).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 index_min [|0; 2; 9; 3; 6|] 2 4 = 4
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 Funkcija [selection_sort_array] implementira urejanje z izbiranjem na mestu. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 Namig: Za testiranje uporabi funkciji [Array.of_list] in [Array.to_list]
 skupaj z [randlist].
[*----------------------------------------------------------------------------*)

