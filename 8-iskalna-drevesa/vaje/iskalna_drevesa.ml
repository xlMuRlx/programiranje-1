(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(*
type 'a tree =
  | Empty
  | Leaf 'a
  | Node of 'a tree * 'a * 'a tree
*)

(* Zato, da ni potrebno podajati definicij tudi za Leaf, je bolje: *)
let leaf x = Node(Empty, x, Empty) 


(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)

let test_tree = 
  let left_tree = Node (leaf 0, 2, Empty) in
  let right_tree = Node (leaf 6, 7, leaf 11) in
  Node (left_tree, 5, right_tree)


(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)

let rec mirror = function
  | Empty -> Empty
  | Node (levi, x, desni) -> Node (mirror desni, x, mirror levi)


(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)

let rec height = function
  | Empty -> 0
  | Node (levi, x, desni) -> 1 + max (height levi) (height desni)

let rec size = function
  | Empty -> 0
  | Node (levi, x, desni) -> 1 + size levi + size desni


(* Repno rekurzivna funkcija size -> poglej pred izpitom. *)
let tl_rec_size tree =
  let rec size' acc queue = 
    (* V queue zapisujemo še nepregledana drevesa. *)
    (* Pogledamo, kateri je naslednji element v vrsti za obravnavo. *)
    match queue with
    | [] -> acc
    | t :: ts -> (
      (* Obravnavamo drevo. *)
      match t with
      | Empty -> size' acc ts (* Prazno drevo samo odstranimo iz vrste. *)
      (* Akumulator povečamo za 1 ter poddrevesa dodamo v vrsto. *)
      | Node (levi, x, desni) -> size' (acc + 1) (levi :: desni :: ts)
    )
  in
  size' 0 [tree]


(*----------------------------------------------------------------------------*]
 Funkcija [follow directions tree] tipa [direction list -> 'a tree -> 'a option]
 sprejme seznam navodil za premikanje po drevesu in vrne vozlišče do katerega 
 vodi podana pot. Ker navodila morda ne vodijo do nobenega vozlišča v drevesu
 vrne rezultat kot [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # follow [Right; Left] test_tree;;
 - : int option = Some 6
 # follow [Right; Left; Right; Right] test_tree;;
 - : int option = None
[*----------------------------------------------------------------------------*)

type direction = Left | Right

let rec follow seznam tree =
  match seznam, tree with
  | [], Node (levi, x, desni) -> Some x
  | _, Empty -> None
  | t :: ts, Node (levi, x, desni) -> (
    match t with
    | Right -> follow ts desni
    | Left -> follow ts levi
  )


(*----------------------------------------------------------------------------*]
 Funkcija [prune directions tree] poišče vozlišče v drevesu [t] glede na 
 navodila, ter izbriše poddrevo, ki se začne v izbranem vozlišču.

 Opozorilo: Pri uporabi [Some Node(l, x, r)] se OCaml pritoži, saj to razume
            kot [(Some Node)(l, x, r)], zato pravilno postavite potrebne 
            oklepaje.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # prune [Right] test_tree;;
 - : int tree option =
 Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty))
[*----------------------------------------------------------------------------*)

let rec prune seznam tree =
  match seznam, tree with 
  | [], Node (levi, x, desni) -> Some (Node (levi, x, desni))
  | _, Empty -> None
  | t :: ts, Node (levi, x, desni) -> (
    match t with
    | Right -> (
      match prune ts desni with
      | None -> Some (Node (levi, x, desni))
      | Some _ -> Some (Node (levi, x, Empty))
    )
    | Left -> (
      match prune ts levi with
      | None -> Some (Node (levi, x, desni))
      | Some _ -> Some (Node (Empty, x, desni))
    )
  )


(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

let rec map_tree f = function
  | Empty -> Empty
  | Node (levi, x, desni) -> Node (map_tree f levi, f x, map_tree f desni)


(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec list_of_tree = function
  | Empty -> []
  | Node (levi, x, desni) -> (list_of_tree levi) @ [x] @ (list_of_tree desni)
  

(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec is_bst tree =
  let rec urejeno = function
    | [] | _ :: [] -> true
    | x :: y :: xs -> if x < y then urejeno (y :: xs) else false
  in
  urejeno (list_of_tree tree)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)


let rec member k tree =
  let rec vsebuje m = function
    | [] -> false
    | x :: xs -> if x = m then true else vsebuje m xs
  in
  vsebuje k (list_of_tree tree)

let rec insert x = function
  | Empty -> leaf x
  | Node (levi, y, desni) -> 
      if y >= x then 
        Node (insert x levi, y, desni) 
      else 
        Node (levi, y, insert x desni)


(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)

let rec member2 k tree =
  let rec vsebuje m = function
    | [] -> false
    | x :: xs -> if x = m then true else vsebuje m xs
  in
  vsebuje k (list_of_tree tree)


(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)

let rec succ = function
  | Empty -> None
  | Node (_, _, desni) -> (
    match list_of_tree desni with 
    | [] -> None
    | x :: xs -> Some x
  )


let rec tree_max = function
  | Empty -> None
  | Node (_, x, Empty) -> Some x
  | Node (_, _, desni) -> tree_max desni

let rec pred = function
  | Empty -> None
  | Node (levi, _, _) -> tree_max levi


(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)

let rec tree_min = function
  | Empty -> None
  | Node (Empty, x, _) -> Some x
  | Node (levi, _, _) -> tree_min levi

let succ2 = function
  | Empty -> None
  | Node(_, _, desni) -> tree_min desni

let rec delete x tree =
  match tree with
  | Empty -> (* Empty case *) Empty
  | Node (Empty, y, Empty) when x = y -> (* Leaf case *) Empty
  | Node (Empty, y, desni) when x = y -> (* One sided *) desni
  | Node (levi, y, Empty) when x = y -> (*One sided *) levi
  | Node (levi, y, desni) when x <> y -> (* Recurse deeper *)
      if x > y then
        Node (levi, y, delete x desni)
      else
        Node (delete x levi, y, desni)
  | Node (levi, y, desni) -> (* SUPER FUN CASE :D *)
      match succ2 tree with 
      | None -> failwith "How is this possible?!" (* This cannot happen :D *)
      | Some z -> Node (levi, z, delete z desni)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type ('key, 'value) dict = ('key * 'value) tree

(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)

let test_dict =
  let desni = Node(leaf ("c", -2), ("d", 2), Empty)
  in
  Node(leaf ("a", 0), ("b", 1), desni)

(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

let rec dict_get key = function
  | Empty -> None
  | Node (levi, (x, y), desni) -> 
      if x = key then Some y
      else
        if x > key then dict_get key levi
        else dict_get key desni
      
  
(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec print_element = function
  | (* prazen *) -> ??
  | (x, y) -> 

let rec print_dict dict =
  let seznam = list_of_tree dict
  in
  match seznam with
  | [] -> print_element (* prazen *)
  | x :: xs -> print_element x 


(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 0
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

