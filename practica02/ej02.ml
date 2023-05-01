# directory "../ocaml-talf/src/"
# load "talf.cma";;
# use "ej01.ml";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

type simb =
    Terminal of string
    | No_terminal of string;;

(* Implemente una funcion cyk : Auto.simbolo list -> Auto.gic -> bool que, dada
una lista de sımbolos de entrada y una gramatica, indique si la cadena de entrada pertenece
o no al lenguaje generado por la gramatica.
Lo primero que debe hacer esta funcion es comprobar que la cadena de entrada tiene al
menos un sımbolo, y que la gramatica esta en forma normal de Chomsky. Si no es ası, la
funcion activara una excepción *)

let gic0 = gic_of_string "S A B C;    a b;  S;  S -> A B | B C;   A -> B A| a;     B -> C C | b; C -> A B | a;";;

let cadena0 = cadena_of_string "b b a b";;
let cadena1 = cadena_of_string "b b a c";;

let rellenamosPrimeraFila matriz cadena =
    let rec aux i j mat cad =
        match cad with
            | [] -> mat
            | simb::tail ->
                mat.(i).(j) <- agregar (simb) mat.(i).(j);
                aux i (j + 1) mat tail
    in aux 0 0 matriz cadena;;

let cyk cadena (Gic(_, _, reglas, axioma) as gic) =
    if (cadena = []) || (not (es_fnc gic)) (* Si está vacía o no está en FNC retornamos una excepción *)
    then raise (Failure "La cadena de esta vacia o la gramatica no esta en FNC")
    else 
        let n = List.length cadena in (* Longitud de la cadena *)
        (* Creamos una matriz de n * n con conjuntos vacíos *)
        let matriz = Array.make_matrix n n conjunto_vacio in
        let matriz = rellenamosPrimeraFila matriz cadena in (* Rellenamos la primera fila con los símbolos de la cadena *)
        
        let rec iterarJ j matriz =
            if j <= n - 1
            then 

                let rec iterarI j i matriz =
                    if i <= n - j - 1
                    then 
                        
                        let rec iterarK j i k matriz reglas = 
                            if k <= i + j - 1
                                then 
                                    
                                    match reglas with
                                        | []  -> iterarI j (i + 1) matriz
                                        | (Regla_gic(a, [b; c]))::tail -> 
                                                    if pertenece b matriz.(i).(k) && pertenece c matriz.(i + k).(j - k) 
                                                    then matriz.(i).(j) <- (agregar a matriz.(i).(j))
                                                    else ();

                                                    iterarK j i (k + 1) matriz (list_of_conjunto (suprimir (Regla_gic(a, [b; c])) (conjunto_of_list tail)))
                                        | (Regla_gic(a, [b]))::tail -> 
                                                    if List.mem b cadena
                                                    then matriz.(i).(j) <- (agregar a matriz.(i).(j))
                                                    else ();

                                                    iterarK j i (k + 1) matriz (list_of_conjunto (suprimir (Regla_gic(a, [b])) (conjunto_of_list tail)))
                                        | _::tail -> iterarK j i (k + 1) matriz tail
                            else iterarI j (i + 1) matriz 
                        in iterarK j i i matriz (list_of_conjunto reglas)

                    else iterarJ (j + 1) matriz
                in iterarI j 0 matriz
            
             else(* pertenece axioma matriz.(0).(n - 1)*)matriz
        in iterarJ 1 matriz;; 
        
cyk cadena0 gic0;;
cyk cadena1 gic0;;
