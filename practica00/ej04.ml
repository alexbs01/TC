type 'a conjunto = Conjunto of 'a list;;

let conjunto_vacio = Conjunto [];;

let conj1 = Conjunto([1;2;3;4;5;6;7;8;9]);;
let conj1_2 = Conjunto([9;6;3;4;1;2;7;8;5]);;
let conj2 = Conjunto([0;2;3;10;8]);;
let conj3 = Conjunto([1;3;8;2]);;

(* Hay que espcificar que c es un parámetro de tipo Conjunto, con tail pasa igual *)
let rec pertenece x = function Conjunto c -> match c with
	| [] -> false
	| head::tail -> if head = x
					then true
					else pertenece x (Conjunto tail);;

let agregar x (Conjunto c) =
	if pertenece x (Conjunto c)
	then Conjunto c
	else Conjunto (x::c);;

let conjunto_of_list l =
	let rec aux (Conjunto c) lista = match lista with
		| [] -> (Conjunto c)
		| head::tail -> aux (agregar head (Conjunto c)) tail	
	in aux (Conjunto []) l;;

(* Se repasan todos los elementos y se van concatenando a acc, cuando el 
   primer elemento del conjunto coincide con x, concatena su cola a acc *)
let suprimir x (Conjunto con) =
	if pertenece x (Conjunto con)
	then 
		let rec aux acc = 
			function (Conjunto c) -> match c with
				| [] -> (Conjunto c)
				| head::tail -> if head = x
								then Conjunto (acc@tail)
								else aux (head::(acc)) (Conjunto tail)
		in aux [] (Conjunto con)
	else (Conjunto con);; (* Si x no pertenece al conjunto muestra el conjunto *)

(* En cada iteración, quita el primer elemento del conjunto y suma uno al 
   parámetro que lleva la cuenta *)
let cardinal (Conjunto con) = 
	let rec aux count = 
		function (Conjunto c) -> match c with
			| [] -> count
			| _::tail -> aux (count + 1) (Conjunto tail)
	in aux 0 (Conjunto con);;

(* Concatena ambas listas y crea un conjunto a partir de la lista resultante *)
let union (Conjunto c1) (Conjunto c2) = match c1, c2 with
	| _, _ -> conjunto_of_list (List.rev_append c1 c2);;
	
(* Comprueba si el primer elemento del conjunto está en el otro, si está lo 
   concatena a acc, sino lo descarta *)
let interseccion (Conjunto con1) (Conjunto con2) = 
	let rec aux (Conjunto c1) (Conjunto c2) acc = match c1, c2 with
		| [], _
		| _, [] -> Conjunto acc
		| head::tail, _ -> if pertenece head (Conjunto c2)
						   then aux (Conjunto tail) (Conjunto c2) (head::acc)
						   else aux (Conjunto tail) (Conjunto c2) (acc)
	in aux (Conjunto con1) (Conjunto con2) [];;
	
(* Comprueba si el primer elemento del conjunto pertenece a con2, si no 
   pertenece lo concatena a acc, sino lo descarta *)	
let diferencia (Conjunto con1) (Conjunto con2) = 
	let rec aux (Conjunto c1) (Conjunto c2) acc = match c1, c2 with
		| [], _
		| _, [] -> Conjunto acc
		| head::tail, _ -> if pertenece head (Conjunto c2)
						   then aux (Conjunto tail) (Conjunto c2) (acc)
						   else aux (Conjunto tail) (Conjunto c2) (head::acc)
	in aux (Conjunto con1) (Conjunto con2) [];;

(* Comprueba que los elementos de con2 estén en con1, si se llega al final 
   es que todos pertenecen a con1 y retorna true, si uno no pertenece 
   retorna false *)
let incluido (Conjunto con1) (Conjunto con2) = 
	let rec aux (Conjunto c1) (Conjunto c2) = match c1, c2 with
		| [], _ -> false
		| _, [] -> true
		| _, head::tail -> if pertenece head (Conjunto c1)
						   then aux (Conjunto c1) (Conjunto tail)
						   else false
	in aux (Conjunto con1) (Conjunto con2);;

(* Primero comprueba si tienen la misma cardinalidad, si lo tienen comprueba si todos los de con1 pertenecen a con2, retorna
    true, si hay uno que no pertenece, retorna false *)
let igual (Conjunto con1) (Conjunto con2) = 
	if (cardinal (Conjunto con1)) = (cardinal (Conjunto con2))
	then let rec aux (Conjunto c1) (Conjunto c2) = match c1, c2 with
			| [], [] -> true
			| [], _::_ -> true
			| head::tail, _ -> if pertenece head (Conjunto c2)
							   then aux (Conjunto tail) (Conjunto c2)
							   else false
							   
		 in aux (Conjunto con1) (Conjunto con2)
	else false;;

(* En cada iteración de iterarCon1 se selecciona la cabecera se utiliza para 
   concatenarla a todos los elementos de con2 y luego se desecha. IterarCon2 
   concatena la cabecera a todos los elementos de con1 *)
let producto_cartesiano (Conjunto con1) (Conjunto con2) = 
	let rec iterarCon1 c1 c2 acc = match c1, c2 with
		| [], _ -> Conjunto acc
		| head::tail, _ -> 
			let rec iterarCon2 head c21 acc = match c21 with
				| [] -> iterarCon1 tail con2 acc
				| head2::tail2 -> if c21 = []
								  then iterarCon1 tail con2 acc
								  else iterarCon2 head tail2  ((head, head2)::acc)
                        
			in iterarCon2 head c2 acc
  
	in iterarCon1 con1 con2 [];;

let list_of_conjunto = function Conjunto c -> c;;

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
