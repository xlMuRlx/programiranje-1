(* ===== PRVA NALOGA ===== *)


(* a) *)
let rec razlika_kvadratov x y = 
	(x + y) * (x + y) - (x * x + y * y)


(* b) *)
let rec uporabi_na_paru f (x, y) =
	(f x, f y)


(* c) *)
let rec ponovi_seznam n sez = 
	if n <= 0 then 
		[]
	else
		sez @ (ponovi_seznam (n-1) sez)
	

(* d) *)
let rec razdeli sez = 
	let rec razdeli' negativni ostali sez =
		match sez with
		| [] -> (List.rev negativni, List.rev ostali)
		| x :: xs -> 
			if x < 0 then
				razdeli' (x :: negativni) ostali xs
			else
				razdeli' negativni (x :: ostali) xs
	in
	razdeli' [] [] sez




(* ===== DRUGA NALOGA ===== *)

type 'a tree =
	| Empty
	| Node of 'a tree * 'a * 'a tree

let leaf x = Node(Empty, x, Empty) 

let test_tree = 
	let left_tree = Node (leaf 3, 10, Node (leaf 14, 13, leaf 6)) in
	let right_tree = Node (leaf 2, 8, leaf 10) in
	Node (left_tree, 11, right_tree)
 

(* Pomožni funkciji *)
let rec padajoca v = function
	| Empty -> []
	| Node (levo, x, desno) when x > v -> []
	| Node (levo, x, desno) -> 
		let left = padajoca x levo in
		let right = padajoca x desno in
		if List.length left > List.length right then
			x :: left
		else
			x :: right


let rec narascajoca v = function
	| Empty -> []
	| Node (levo, x, desno) when x < v -> []
	| Node (levo, x, desno) -> 
		let left = narascajoca x levo in
		let right = narascajoca x desno in
		if List.length left > List.length right then
			x :: left
		else
			x :: right

(* Sestavimo končno funkcijo *)
let rec monotona_pot = function
	| Empty -> []
	| Node (levi, x, desni) ->
		(* Recursiva search for paths. *)
		let pure_left = monotona_pot levi in
		let pure_right = monotona_pot desni in
		let left_to_right = (List.rev (padajoca x levi)) @ [x] @ (narascajoca x desni) in
		let right_to_left = (List.rev(padajoca x desni)) @ [x] @ (narascajoca x levi) in
		(* Choose the longest one. *)
		let options = [pure_left; pure_right; left_to_right; right_to_left] in
		let izberi_najdaljso x y =
			if List.length x > List.length y then x else y
		in
		List.fold_left izberi_najdaljso pure_left options
		(* Fold_left zaporedoma gleda elemente seznama *)




(* ===== TRETJA NALOGA ===== *)

type 'a veriga = 
	| Filter of ('a -> bool) * 'a list * 'a veriga
	| Ostalo of 'a list


(* a) *)
let test =
	Filter ((fun x -> x < 0), [],
	Filter ((fun x -> x < 10), [],
	Ostalo []))


(* b) *)
let rec vstavi x veriga =
	match veriga with
	| Ostalo elementi -> Ostalo (x :: elementi)
	| Filter (f, elementi, filtri) ->
		if f x then 
			Filter (f, x :: elementi, filtri)
		else
			Filter (f, elementi, vstavi x filtri)


(* c) *)
let rec poisci x = function
	| Ostalo elementi -> List.mem x elementi 
	(* mem vrne true če je x element seznama *)
	| Filter (f, elementi, filtri) ->
		if f x then
			List.mem x elementi
		else
			poisci x filtri


(* d) *)
let rec izprazni_filtre = function
	| Ostalo elementi -> (Ostalo [], elementi)
	| Filter (f, elementi, filtri) -> 
		let prazni_filtri, pobrani_elementi = izprazni_filtre filtri in
		let vsi_elementi = elementi @ pobrani_elementi in
		(Filter(f, [], prazni_filtri), vsi_elementi)


(* e) *)
let rec dodaj_filter f veriga =
	let veriga' = Filter(f, [], veriga) in
	let prazna_veriga, elementi = izprazni_filtre veriga' in
	List.fold_left (fun v x -> vstavi x v) prazna_veriga elementi


	