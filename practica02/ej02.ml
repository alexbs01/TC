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

let gic1 = gic_of_string "S A B;    a b c;  S;  S -> a A;   A -> a b c A | b B;     B -> b c B | a;";;

let gic2 = gic_of_string "S A B;    a b c;  S;  S -> A;   A -> B | b;     B -> A B | a | c;";;

let cadena0 = cadena_of_string "";;

let cadena1 = cadena_of_string "a b c";;

let cadena2 = cadena_of_string "b a";;

let cadena3 = cadena_of_string "a a";;

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
            if j = n - 1
            then 
                if pertenece axioma matriz.(1).(n)
                then true
                else false

            else
                let rec iterarI j i matriz =
                    if i = (n - j)
                    then iterarJ (j + 1) matriz
                    else 
                        let rec iterarK j i k matriz reglas = 
                            match reglas with
                                | conjunto_vacio -> iterarI j (i + 1) matriz
                                | (Regla_gic(No_terminal a, [No_terminal b; No_terminal c])) -> 
                                    if pertenece b matriz.(i).(k) && pertenece c matriz.(i + k).(j - k)
                                    then matriz.(i).(j) <- agregar (No_terminal a) matriz.(i).(j)
                                    else iterarK j i k (suprimir (No_terminal a) reglas)
                        in iterarK j i 0 matriz reglas
                in iterarI j 0 matriz
        in iterarJ 1 matriz;;
              


cyk cadena0 gic2
cyk cadena1 gic1
cyk cadena2 gic2
cyk cadena3 gic2

