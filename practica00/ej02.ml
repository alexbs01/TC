(* Si head cumple f, retorna head, sino descarta la cabecera*)
let rec primero_que_cumple f l = match l with
	| [] -> None
	| head::tail -> if f head
					then Some head
					else primero_que_cumple f tail;;
					
(* val primero_que_cumple : ('a -> bool) -> 'a list -> 'a option = <fun> *)

(* primero_que_cumple (fun x -> x mod 2 = 0) [1;3;4;5;6];; *)

(* Si primero que cumple retorna None, es que no hay ningún elemento que cumpla f*)
let existe f l =  match primero_que_cumple f l with 
	| None -> false
	| Some x -> true;;
	

let asociado conj key = 
	let aux = 
		primero_que_cumple (function (a, b) -> if a = key 
											   then true 
											   else false) conj in
		match aux with
			| None -> raise(Not_found)
			| Some (x, y) -> y

(* asociado [(1, 2); (2, 4); (3, 6); (4, 8)] 3;; *)
