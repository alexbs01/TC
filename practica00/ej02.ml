(* Si head cumple f, retorna head, sino descarta la cabecera*)
let rec primero_que_cumple f l = match l with
	| [] -> []
	| head::tail -> if f head 
					then [head]
					else primero_que_cumple f tail;;
					
(* val primero_que_cumple : ('a -> bool) -> 'a list -> 'a list = <fun> *)

(* Si la lista está vacía, es que no hay ningún elemento que cumpla f*)
let existe f l = 
	if primero_que_cumple f l = []
	then false 
	else true;;
	
(* Va recorriendo la lista, si llega a un x que coincide con la key, retorna y*)
let rec asociado conj key = match conj with
	| [] -> List.hd []
	| (x, y)::tail -> if x = key
					  then List.hd [y]
					  else asociado tail key;;
