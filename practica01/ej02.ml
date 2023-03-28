# directory "../ocaml-talf/src/"
# load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

(* -------------------------- *)

let afd1 = af_of_string "0 1 2; 	a b; 	0; 	2;	0 1 a; 1 1 b; 1 2 a;";;

let afd2 = af_of_string "1 2 3;		a b; 	1; 	3; 	1 2 a; 2 2 b; 2 3 a;";;



(* Autómata no determinista con dos epsilon-transiciones en el mismo estado *)
let afn1 = af_of_string "0 1 2 3; 		a b c; 		0; 	1 3; 	0 1 a; 1 1 b; 1 2 a; 2 0 epsilon; 2 3 epsilon; 2 3 c;";;

(* Autómata no determinista con dos epsilon-transiciones y dos símbolos iguales en el mismo estado *)
let afn2 = af_of_string "0 1 2 3 4; 	a b c; 		0; 	1 3; 	0 1 a; 1 1 b; 1 2 a; 2 0 epsilon; 2 3 epsilon; 2 3 c; 1 4 a; 4 3 c;";;

(* Autómata no determinista con epsilon-transiciones sin símbolos repetidos en el mismo estado *)
let afn3 = af_of_string "0 1 2 3 4 5; 	a b c d; 	0; 	1 3 5; 	0 1 a; 0 5 d; 2 2 a; 2 3 epsilon; 2 3 b; 2 3 c; 1 3 a; 5 0 epsilon; 5 2 d;";;


(* Implemente una función equivalentes : Auto.af -> Auto.af -> bool que reciba como
argumentos dos autómatas finitos y que devuelva true cuando ambos autómatas acepten el mismo
lenguaje, o false en caso contrario. *)

(* Dados dos autómatas y un par de estados, si uno de los pares es final y el otro no retorna true *)
let unoEsFinalYElOtroNo (Af (_, _, _, _, (Conjunto finales1)))
						(Af (_, _, _, _, (Conjunto finales2)))
						(posibleFinal1, posibleFinal2) =
					
	if ((pertenece posibleFinal1 (Conjunto finales1)) && 
	   (not (pertenece posibleFinal2 (Conjunto finales2)))) || 
		((not (pertenece posibleFinal1 (Conjunto finales1))) && 
	   (pertenece posibleFinal2 (Conjunto finales2)))
	then true
	else false
	
(* let transiciona	(Af (_, _, _, (Conjunto transiciones), _)) (Estado estado) (Terminal simbolo) =  *)
	(* let rec aux = match transiciones with *)
		(* | Arco_af ((Estado origen), (Estado destino), (Terminal simb))::tail -> if (estado = origen) && (simbolo = simb) *)
																				(* then destino *)
																				(* else aux tail (Estado estado) (Terminal simbolo) *)
	(* in aux transiciones (Estado estado) (Terminal simbolo) *)

type simbolo =
	| Terminal of string
	| No_terminal of string

type origen = 
	| Estado of string

type destino = 
	| Estado of string

type estado = 
	| Estado of string

(* transiciona: Retorna una lista de estados destino a los que se puede acceder desde un origen usando un simbolo *)
let transiciona (Af (_, _, _, (Conjunto transiciones), _)) estado simbolo =
	let rec aux acc transiciones state simb = match transiciones with
		| Arco_af (origen, destino, simb)::tail -> 
			if (state = origen) && (simb = simbolo) 
			then aux (destino::acc) tail state simbolo
			else aux acc tail state simb
		| [] -> acc
	in aux [] transiciones estado simbolo


(* let equivalentes (Af (estados1, alfabeto, inicial1, trans1, finales1) as af1) *)
                 (* (Af (estados2, _, inicial2, trans2, finales2) as af2) = *)

	(* let rec aux visitados cola =  *)
		(* if cola = [] *)
		(* then true *)
		(* else if (pertenece (List.hd cola) visitados) *)
			 (* then aux visitados (List.tl cola) *)
			 (* else if unoEsFinalYElOtroNo af1 af2 (List.hd cola) *)
				  (* then false *)
				  (* else  *)
				  
	
	(* in aux conjunto_vacio [(inicial1, inicial2)] *)
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	