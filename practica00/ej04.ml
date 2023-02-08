type 'a conjunto = Conjunto of 'a list;;

let conjunto_vacio = Conjunto [];;

let conj = Conjunto([1;2;3;4;5;6;7;8;9]);;

let rec pertenece x c = match c with
	| [] -> false
	| head::tail -> if head = x
					then true
					else pertenece x tail;;
	
(*conjunto_of_list: Una lista puede ir con repetidos, hay que gestionarlos*)

(*incluido a b: a tiene que estar en b*)

(*
pertenece : 'a -> 'a conjunto -> bool
agregar : 'a -> 'a conjunto -> 'a conjunto
conjunto_of_list : 'a list -> 'a conjunto
suprimir : 'a -> 'a conjunto -> 'a conjunto
cardinal : 'a conjunto -> int
union : 'a conjunto -> 'a conjunto -> 'a conjunto
interseccion : 'a conjunto -> 'a conjunto -> 'a conjunto
diferencia : 'a conjunto -> 'a conjunto -> 'a conjunto
incluido : 'a conjunto -> 'a conjunto -> bool
igual : 'a conjunto -> 'a conjunto -> bool
producto_cartesiano : 'a conjunto -> 'b conjunto -> ('a * 'b) conjunto
list_of_conjunto : 'a conjunto -> 'a list
*)
