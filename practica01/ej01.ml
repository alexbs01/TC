# directory "../ocaml-talf/src/"
# load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

(* Estados; Símbolos; Estado inicial; Estados de aceptación; (Estado origen Estado destino Símbolo;)^+  *)

(* Autómata determinista *)
let afd1 = af_of_string "0 1 2; 		a b c d; 	0; 	1;		0 1 a; 1 1 b; 1 2 a; 2 0 d; 2 1 c; 0 2 d;";;

(* Autómata determinista *)
let afd2 = af_of_string "0 1;			a b c; 		0; 	1; 		0 1 a; 1 1 b; 1 1 a; 1 0 c;";;

(* Autómata no determinista con dos epsilon-transiciones en el mismo estado *)
let afn1 = af_of_string "0 1 2 3; 		a b c; 		0; 	1 3; 	0 1 a; 1 1 b; 1 2 a; 2 0 epsilon; 2 3 epsilon; 2 3 c;";;

(* Autómata no determinista con dos epsilon-transiciones y dos símbolos iguales en el mismo estado *)
let afn2 = af_of_string "0 1 2 3 4; 		a b c; 		0; 	1 3; 	0 1 a; 1 1 b; 1 2 a; 2 0 epsilon; 2 3 epsilon; 2 3 c; 1 4 a; 4 3 c;";;

(* Autómata no determinista con epsilon-transiciones sin símbolos repetidos en el mismo estado *)
let afn3 = af_of_string "0 1 2 3 4 5; 	a b c d; 	0; 	1 3 5; 	0 1 a; 0 5 d; 2 2 a; 2 3 epsilon; 2 3 b; 2 3 c; 1 3 a; 5 0 epsilon; 5 2 d;";;


(* let getTransiciones = function (Af (_, _, _, Conjunto transiciones, _)) -> transiciones;; *)

(*Implemente una función es_afne : Auto.af -> bool que reciba como argumento un autómata
finito, y que devuelva true si se trata de un autómata que presenta alguna épsilon-transición, o false en
caso contrario.*)
let es_afne = function Af (_, _, _, (Conjunto transiciones), _) -> (* Obtenemos el conjunto de transiciones *)
	let rec aux = function
	
		| [] -> false (* Si está vacío retornamos false *)
		| Arco_af (_, _, simbolo)::tail -> (* Seleccionamos la lista de Arco_af*)
		
			if simbolo = Terminal "" (*Si el símbolo de la cabecera es epsilo retornamos true*)
			then true
			else aux tail (* Si no es epsilon, repetimos aux con la cola*)
			
	in aux transiciones;;

(* *********************** *)

(* Implemente una función es_afn : Auto.af -> bool que reciba como argumento un autómata
finito, y que devuelva true si se trata de un autómata que presenta algún tipo de no determinismo
(excepto épsilon-transiciones), o false en caso contrario. *)
let es_afn = function Af (_, _, _, (Conjunto transiciones), _) -> 
	let rec aux acc = function
	
		| [] -> false
		| Arco_af (origen, _, simbolo)::tail -> (* Seleccionamos el origen y el simbolo de la transición *)
		
			if (not (pertenece (origen, simbolo) acc)) (* Si no pertenece, la añadimos a la lista de acumulados *)
			then aux (agregar (origen, simbolo) acc) tail 
			else 
				if (simbolo = Terminal "") (* Si pertenece y el símbolo es epsilon terminal*)
				then aux acc tail (* No lo agregamos pero continuamos el programa *)
				else true (* Si el símbolo no es epsilon retornamos true *)
		
	in aux (Conjunto []) transiciones;;
	
(* *********************** *)

(* Implemente una función es_afd : Auto.af -> bool que reciba como argumento un autómata
finito, y que devuelva true si se trata de un autómata totalmente determinista, o false en caso contrario *)
let es_afd = function automata ->
	if es_afne automata || es_afn automata (* Si el autómata tiene epsilon transiciones o tiene no determinismos *)
	then false
	else true;;
