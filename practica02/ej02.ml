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

let intercambiarPorNoTerminales simbolo reglas =
    let rec aux acc sim reg =
        match reg with
            | (Regla_gic(valor, listaSimbolos))::tl -> if List.mem sim listaSimbolos
                                                       then aux (agregar valor acc) sim tl
                                                       else aux acc sim tl
            | [] -> acc
    in aux conjunto_vacio simbolo (list_of_conjunto reglas);;

let rellenamosPrimeraFila matriz cadena reglas =
    let rec aux i j mat cad =
        match cad with
            | [] -> mat
            | simb::tail ->
                mat.(i).(j) <- union (intercambiarPorNoTerminales simb reglas) mat.(i).(j);
                aux i (j + 1) mat tail
    in aux 0 0 matriz cadena;;

let cyk cadena (Gic(_, _, reglas, axioma) as gic) =
    if (cadena = []) || (not (es_fnc gic)) (* Si está vacía o no está en FNC retornamos una excepción *)
    then raise (Failure "La cadena de esta vacia o la gramatica no esta en FNC")
    else 
        let n = List.length cadena in (* Longitud de la cadena *)
        (* Creamos una matriz de n * n con conjuntos vacíos *)
        let matriz = Array.make_matrix n n conjunto_vacio in
        let matriz = rellenamosPrimeraFila matriz cadena reglas in (* Rellenamos la primera fila con los símbolos de la cadena *)
        
        for j = 1 to n - 1 do
            for i = 0 to n - j - 1 do
                (* let simbolos = ref [] in  *)
                for k = i to i + j - 1 do
                    let rec iterarReglas reglas =
                        if List.length reglas > 0
                        then
                            match reglas with
                                | (Regla_gic(valor, listaSimbolos))::tl -> match listaSimbolos with
                                    | [a; b] -> if (pertenece a matriz.(i).(k)) && (pertenece b matriz.(k + 1).(i + j))
                                                then matriz.(i).(j) <- union (agregar valor matriz.(i).(j)) matriz.(i).(j)
                                                else iterarReglas tl
                                | _ -> iterarReglas tl
                        else ()
                    in iterarReglas (list_of_conjunto reglas);
                done
            done
        done;
        (* pertenece axioma matriz.(0).(n);; *)
        matriz;;

        
cyk cadena0 gic0;;
