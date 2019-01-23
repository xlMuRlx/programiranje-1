(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree = 
  | Empty
  | Node of 'a tree * 'a * 'a tree


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
  let levi = Node(leaf 0, 2, Empty) in
  let desni = Node(leaf 6, 7, leaf 11) in
  Node(levi, 5, desni)


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
  | Node(levi, x, desni) -> Node(mirror desni, x, mirror levi)


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
  | Node(levi, x, desni) -> 1 + max (height levi) (height desni)


let rec size = function
  | Empty -> 0
  | Node(levi, x, desni) -> 1 + size levi + size desni


let rec repno_size tree =
  let rec size' acc queue = 
    match queue with
    | [] -> acc
    | t :: ts -> (
      match t with
      | Empty -> size' acc ts
      | Node(levi, x, desni) -> size' (acc + 1) (levi :: desni :: ts)
    )
  in
  size' 0 [tree]


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
  | Node(levi, x, desni) -> Node(map_tree f levi, f x, map_tree f desni)


(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec list_of_tree = function
  | Empty -> []
  | Node(levi, x, desni) -> (list_of_tree levi) @ [x] @ (list_of_tree desni)


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
    | [] -> true
    | _ :: [] -> true
    | x :: y :: xs -> 
      if x < y then
        urejeno (y :: xs)
      else
        false
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

let rec member x = function
  | Empty -> false
  | Node(levi, y, desni) when (x = y) -> true
  | Node(levi, y, desni) ->
    if y > x then
      member x levi
    else 
      member x desni
  

let rec insert y = function
  | Empty -> leaf y
  | Node(levi, x, desni) ->
    if y < x then
      Node(insert y levi, x, desni)
    else
      Node(levi, x, insert y desni)


(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)

let rec member2 x = function
  | Empty -> false
  | Node(levi, y, desni) when (x = y) -> true
  | Node(levi, y, desni) ->
    if member x levi then
      true
    else
      if member x desni then
        true
      else 
        false


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

let rec succ tree =
  let rec najmanjsi = function
    | [] -> None
    | x :: xs -> Some x
  in
  match tree with
  | Empty -> None
  | Node(_, _, desni) -> najmanjsi (list_of_tree desni)


let rec pred tree =
  let rec najvecji = function
    | [] -> None
    | x :: [] -> Some x
    | _ :: xs -> najvecji xs
  in
  match tree with
  | Empty -> None
  | Node(levi, _, _) -> najvecji (list_of_tree levi)


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

let rec delete x tree =
  match tree with
  | Empty -> Empty
  | Node(Empty, y, Empty) when (x = y) -> Empty
  | Node(Empty, y, desni) when (x = y) -> desni
  | Node(levi, y, Empty) when (x = y) -> levi
  | Node (levi, y, desni) when (x <> y) ->
    if y > x then
      Node(delete x levi, y, desni)
    else
      Node(levi, y, delete x desni)
  | Node (levi, y, desni) -> (
    match succ tree with
    | None -> failwith "Se ne zgodi!"
    | Some z -> Node(levi, z, delete z desni)
  )


let rec delete2 x tree =
  match tree with
  | Empty -> Empty
  | Node(Empty, y, Empty) when (x = y) -> Empty
  | Node(Empty, y, desni) when (x = y) -> desni
  | Node(levi, y, Empty) when (x = y) -> levi
  | Node (levi, y, desni) when (x <> y) ->
    if y > x then
      Node(delete x levi, y, desni)
    else
      Node(levi, y, delete x desni)
  | Node (levi, y, desni) -> (
    match pred tree with
    | None -> failwith "Se ne zgodi!"
    | Some z -> Node(delete z levi, z, desni)
  )


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
  let desni = Node(leaf ("c", -2), ("d", 2), Empty) in
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
  | Node(_, (x, y), _) when (key = x) -> Some y
  | Node(levi, (x, y), desni) -> 
    if key > x then
      dict_get key desni
    else
      dict_get key levi


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

let rec print_dict dict =
  let rec print_list = function
    | [] -> ()
    | (x, y) :: xs -> (
      print_string (x ^ " : "); print_int y; print_newline();
      print_list xs
    )
  in
  print_list (list_of_tree dict)


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

let rec dict_insert key value = function
  | Empty -> leaf (key, value)
  | Node(levi, (x, y), desni) when (x = key) -> Node(levi, (x, value), desni)
  | Node(levi, (x, y), desni) ->
    if key > x then
      Node(levi, (x, y), dict_insert key value desni)
    else
      Node(dict_insert key value levi, (x, y), desni)


(*----------------------------------------------------------------------------*]
 Funkcija [bst_of_list] iz seznama naredi dvojiško iskalno drevo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # [11; 6; 7; 0; 2; 5] |> bst_of_list |> is_bst;;
 - : bool = true
[*----------------------------------------------------------------------------*)

let rec insert x = function
  | Empty -> leaf x
  | Node(levi, y, desni) ->
    if x < y then
      Node(insert x levi, y, desni)
    else
      Node(levi, y, insert x desni)


let rec bst_of_list seznam =
  let rec bst' tree sez =
    match List.rev sez with
    | [] -> tree
    | x :: xs -> bst' (insert x tree) xs
  in
  bst' Empty seznam


(*----------------------------------------------------------------------------*]
 Funkcija [tree_sort] uredi seznam s pomočjo pretvorbe v bst in nato nazaj
 v seznam.

 Opomba: Prosim ne uporabljajte te funkcije v praksi.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # tree_sort ["a"; "c"; "f"; "b"; "e"; "d"];;
 - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
[*----------------------------------------------------------------------------*)

let rec tree_sort seznam =
  let tree = bst_of_list seznam in
  list_of_tree tree


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

type direction =
  | Right
  | Left


let rec follow directions tree =
  match tree with
  | Empty -> None
  | Node(levi, x, desni) -> (
    match directions with
    | [] -> Some x
    | t :: ts -> (
      match t with
      | Right -> follow ts desni
      | Left -> follow ts levi
    )
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

let rec prune directions tree =
  match directions, tree with
  | _, Empty -> None
  | [], Node(levi, x, desni) -> Some Empty
  | t :: ts, Node(levi, x ,desni) -> (
    match t with
    | Right -> (
      match prune ts desni with
      | None -> None
      | Some novi -> Some(Node(levi, x, novi))
    )
    | Left -> (
      match prune ts levi with
      | None -> None
      | Some novi -> Some(Node(novi, x, desni))
    )
  )


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 PHANTOM TREES

 Druga možnost pri brisanju podatkov je, da spremenimo tip s katerim
 predstavljamo drevo. Definirate nov tip fantomskega drevesa, ki poleg podatka,
 levega in desnega poddrevesa hrani še dodatno informacijo o stanju [state], ki
 je bodisi [Exists] če je vozlišče še prisotno in pa [Ghost] če je vozlišče v
 drevesu izbrisano in ga upoštevamo le še kot delitveno vozlišče. Še vedno
 predpostavljamo, da imajo drevesa obliko BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type state =
  | Exists
  | Ghost

type 'a phantom_tree = 
  | P_Empty
  | P_Node of 'a phantom_tree * 'a * 'a phantom_tree * state


(*----------------------------------------------------------------------------*]
 Funkcija [phantomize] tipa ['a tree -> 'a phantom_tree] navadnemu drevesu
 priredi ekvivalentno fantomsko drevo.
 Funkcija [kill x ptree] izbriše element [x] v fantomskem drevesu tako, da 
 njegovo stanje nastavi na [Ghost].
 Predpostavite lahko, da v drevesu ni ponovitev elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # phantomize test_tree;;
 - : int phantom_tree =
 P_Node (P_Node (P_Node (P_Empty, 0, P_Empty, Exists), 2, P_Empty, Exists), 5,
 P_Node (P_Node (P_Empty, 6, P_Empty, Exists), 7,
 P_Node (P_Empty, 11, P_Empty, Exists), Exists),
 Exists)

 # bst_of_list [3; 4; 2] |> phantomize |> kill 3 |> kill 6;;
 - : int phantom_tree =
 P_Node (P_Empty, 2,
 P_Node (P_Node (P_Empty, 3, P_Empty, Ghost), 4, P_Empty, Exists), Exists)
[*----------------------------------------------------------------------------*)

let rec phantomize = function
  | Empty -> P_Empty
  | Node(levi, x, desni) -> P_Node(phantomize levi, x, phantomize desni, Exists)


let rec kill x = function
  | P_Empty -> P_Empty
  | P_Node(levi, y, desni, status) when (y = x) -> P_Node(levi, y, desni, Ghost)
  | P_Node(levi, y, desni, status) ->
    if y > x then
      P_Node(kill x levi, y, desni, status)
    else
    P_Node(levi, y, kill x desni, status)


(*----------------------------------------------------------------------------*]
 Funkcija [unphantomize] tipa ['a phantom_tree -> 'a tree] fantomskemu drevesu 
 priredi navadno drevo, ki vsebuje zgolj vozlišča, ki še obstajajo. Vrstni red
 vozlišč v končnem drevesu ni pomemben.

 Namig: Lahko uporabite vmesni prehodom na drugo podatkovno strukturo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize;;
 - : int tree = Node (Node (Node (Empty, 2, Empty), 6, Empty), 11, Empty)
[*----------------------------------------------------------------------------*)

let rec unphantomize ptree =
  let rec list_of_ptree = function
  | P_Empty -> []
  | P_Node(levi, x, desni, Exists) -> (list_of_ptree levi) @ [x] @ (list_of_ptree desni)
  | P_Node(levi, x, desni, Ghost) -> (list_of_ptree levi) @ (list_of_ptree desni)
  in
  let seznam = list_of_ptree ptree in
  bst_of_list seznam