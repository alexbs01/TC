# directory "../ocaml-talf/src/"
# load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

(* -------------------------- *)

type simbolo =
	| Terminal of string
	| No_terminal of string;;

type origen = 
	| Estado of string;;

type destino = 
	| Estado of string;;

type estado = 
	| Estado of string;;

let afd1 = af_of_string "0 1 2; 	a b; 	0; 	2;	0 1 a; 1 1 b; 1 2 a;";;

let afd2 = af_of_string "1 2 3;		a b; 	1; 	3; 	1 2 a; 2 2 b; 2 3 a;";;

let afd3 = af_of_string "0 1 2; 	    a b; 	0; 	0 2;    0 1 a; 1 1 a; 1 2 b; 2 2 a; 2 2 b;";;

let afd4 = af_of_string "0 1 2 3;    a b;   0;     0 2 3;     0 1 a; 1 2 b; 2 3 a; 3 3 a; 3 3 b; 1 1 a; 2 2 b;";;

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

(* transiciona: Retorna una lista de estados destino a los que se puede acceder desde un origen usando un simbolo *)
let transiciona (Conjunto transiciones) estado simbolo =
	let rec aux transiciones state simb = match transiciones with
		| Arco_af (origen, destino, simb)::tail -> 
			if (state = origen) && (simb = simbolo) 
			then destino
			else aux tail state simb
      | [] -> Estado "NULL"
	in aux transiciones estado simbolo;;

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

                let rec aux2 alfab cola2 visitados2 =
                if alfab = [] (* Si la lista del alfabeto queda vacía, ejecuta mientrasColaNoVacia con el nuevo conjunto de visitados y la cola con los siguientes estados a visitar *)
                then mientrasColaNoVacia visitados2 cola2
                else 

                    let (nuevoEstado1, nuevoEstado2) = (transiciona trans1 estadoActual1 (List.hd alfab), transiciona trans2 estadoActual2 (List.hd alfab)) in
                    if nuevoEstado1 = Estado "NULL" && nuevoEstado2 = Estado "NULL" (* Si ambos estados son NULL, no se agrega a la cola *)
                    then aux2 (List.tl alfab) cola2 visitados2
                    else aux2 (List.tl alfab) (List.rev_append (List.rev cola2) [(nuevoEstado1, nuevoEstado2)]) visitados2
        
                in aux2 (list_of_conjunto alfabeto) cola (agregar (estadoActual1, estadoActual2) visitados)

in mientrasColaNoVacia conjunto_vacio [(inicial1, inicial2)];;

equivalentes afd1 afd2;;
equivalentes afd1 afd3;;
equivalentes afd3 afd4;;
