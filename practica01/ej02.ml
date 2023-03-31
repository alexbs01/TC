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
let unoEsFinalYElOtroNo (Conjunto finales1)
                         (Conjunto finales2)
                         (posibleFinal1, posibleFinal2) =
					
	if ((pertenece posibleFinal1 (Conjunto finales1)) && 
	   (not (pertenece posibleFinal2 (Conjunto finales2)))) || 
		((not (pertenece posibleFinal1 (Conjunto finales1))) && 
	   (pertenece posibleFinal2 (Conjunto finales2)))
	then true
	else false;;
	
(* let transiciona	(Af (_, _, _, (Conjunto transiciones), _)) (Estado estado) (Terminal simbolo) =  *)
	(* let rec aux = match transiciones with *)
		(* | Arco_af ((Estado origen), (Estado destino), (Terminal simb))::tail -> if (estado = origen) && (simbolo = simb) *)
																				(* then destino *)
																				(* else aux tail (Estado estado) (Terminal simbolo) *)
	(* in aux transiciones (Estado estado) (Terminal simbolo) *)

type simbolo =
	| Terminal of string
	| No_terminal of string;;

type origen = 
	| Estado of string;;

type destino = 
	| Estado of string;;

type estado = 
	| Estado of string;;

(* transiciona: Retorna una lista de estados destino a los que se puede acceder desde un origen usando un simbolo *)
let transiciona (Conjunto transiciones) estado simbolo =
	let rec aux transiciones state simb = match transiciones with
		| Arco_af (origen, destino, simb)::tail -> 
			if (state = origen) && (simb = simbolo) 
			then destino
			else aux tail state simb
      | [] -> Estado "NULL"
	in aux transiciones estado simbolo;;

  let afd1 = af_of_string "0 1 2; 	a b; 	0; 	2;	0 1 a; 1 1 b; 1 2 a;";;
let afd2 = af_of_string "1 2 3;		a b; 	1; 	3; 	1 2 a; 2 2 b; 2 3 a;";;
(* Autómata no determinista con dos epsilon-transiciones en el mismo estado *)
let afn1 = af_of_string "0 1 2 3; 		a b c; 		0; 	1 3; 	0 1 a; 1 1 b; 1 2 a; 2 0 epsilon; 2 3 epsilon; 2 3 c;";;
(* Autómata no determinista con dos epsilon-transiciones y dos símbolos iguales en el mismo estado *)
let afn2 = af_of_string "0 1 2 3 4; 	a b c; 		0; 	1 3; 	0 1 a; 1 1 b; 1 2 a; 2 0 epsilon; 2 3 epsilon; 2 3 c; 1 4 a; 4 3 c;";;


let equivalentes (Af (estados1, alfabeto, inicial1, trans1, finales1))
                 (Af (estados2, _, inicial2, trans2, finales2)) =
  let rec mientrasColaNoVacia visitados cola =
    if cola = [] (* Si la cola esta vacía es que se recorrió todo el grafo y es que son idénticos *)
    then true 
    else 
      let (estadoActual1, estadoActual2) = List.hd cola in

        if pertenece (estadoActual1, estadoActual2) visitados (* Si el estado actual ya fue visitado, no se agrega a la cola *)
        then mientrasColaNoVacia visitados (List.tl cola) 
        else 
          
          if unoEsFinalYElOtroNo finales1 finales2 (estadoActual1, estadoActual2) (* Si uno es final y el otro no, no son equivalentes *)
          then false
          else 

            let visitados2 = agregar (estadoActual1, estadoActual2) visitados in (* Se agrega el estado actual a los visitados *)
              let rec aux2 alfab cola2 = (* Se recorre el alfabeto *)
                if alfab = [] (* Si se recorrió todo el alfabeto, se pasa a la siguiente iteración *)
                then mientrasColaNoVacia visitados2 (List.tl cola)
                else 

                  if (transiciona trans1 estadoActual1 (List.hd alfab)) = (transiciona trans2 estadoActual2 (List.hd alfab)) (* Si la transición es la misma, no se agrega a la cola *)
                  then aux2 (List.tl alfab) cola2
                  else aux2 (List.tl alfab) (cola2 @ [(transiciona trans1 estadoActual1 (List.hd alfab), transiciona trans2 estadoActual2 (List.hd alfab))] )
              in aux2 (list_of_conjunto alfabeto) cola

in mientrasColaNoVacia conjunto_vacio [(inicial1, inicial2)];;



equivalentes afd1 afd2;;
equivalentes afn1 afd2;;
