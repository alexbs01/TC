# directory "../ocaml-talf/src/"
# load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

(* Implemente una funcion es_fnc : Auto.gic -> bool que indique si una gramatica dada
esta o no en formal normal de Chomsky. *)

let gic0 = gic_of_string "S A B C;    a b;  S;  S -> A B | B C;   A -> B A| a;     B -> C C | b; C -> A B | a;";;

let gic1 = gic_of_string "S A B;    a b c;  S;  S -> a A;   A -> a b c A | b B;     B -> b c B | a;";;

let es_fnc (Gic(_, _, reglas, _)) = 
	let rec aux = function
		| [] -> true (* Si después de todas las iteraciones, la lista queda vacía, es que esta en FNC *)
		| (Regla_gic(_, listaSimbolos))::tl -> match listaSimbolos with (* Si la lista está llena, obtenemos la regla N de la gramática *)

			| [] -> aux tl (* Si esa regla está vacía continuamos iterando *)
			| lista -> match lista with (* Y ahora comprobamos si la lista cumple lo necesario para ser FNC, si no lo cumple retornamos false *)
                        (* Para que esté en FNC, debe ser o un Terminal, un No_terminal o dos No_terminales. En caso contrario no está en FNC *)
				        | [Terminal _] -> aux tl
					    | [No_terminal _; No_terminal _] -> aux tl
					    | [No_terminal _]
					    | _ -> false
	in aux (list_of_conjunto reglas);; (* Obtenemos las reglas de la gramática y las convertimos en una lista *)

es_fnc gic0;;
es_fnc gic1;;

