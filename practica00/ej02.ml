let rec primero_que_cumple f l = match l with
	| [] -> []
	| head::tail -> if f head
					then [head]
					else primero_que_cumple f tail;;
					
(* val primero_que_cumple : ('a -> bool) -> 'a list -> 'a list = <fun> *)

let existe f l = 
	if primero_que_cumple f l = []
	then false
	else true;;
	
let rec asociado conj key = match conj with
	| [] -> List.hd []
	| (x, y)::t -> 	if x = key
					then List.hd [y]
					else asociado t key;;
