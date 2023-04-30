# directory "../ocaml-talf/src/"
# load "talf.cma";;
# use "ej01.ml";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

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
      | head::tail ->
          mat.(i).(j) <- agregar (Terminal head) conjunto_vacio;
          aux i (j+1) mat tail
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
        then if pertenece axioma matriz.(0).(j)
                then true
                else false
        else 
            let rec iterarI i j matriz reglas =
            if i = (n - j)
            then iterarJ j matriz
            else
                let matriz_actualizada = Array.copy matriz in
                let rec iterarK matriz j i k reglas = 
                match reglas with
                | [] -> matriz
                | (Regla_gic(No_terminal a, [No_terminal b; No_terminal c]))::tail -> 
                    if pertenece b matriz.(i).(k) && pertenece c matriz.(i + k).(j - k)
                    then matriz_actualizada.(i).(j) <- agregar a matriz.(i).(j)
                    else ();
                    iterarK matriz_actualizada j i k tail
                in iterarK matriz_actualizada j i 0 reglas
            in iterarI j 0 matriz reglas
        in iterarJ 1 matriz;;
              


cyk cadena0 gic2;;
cyk cadena1 gic1;;
cyk cadena2 gic2;;
cyk cadena3 gic2;;


(* 1. Se inicializa la primera fila de la tabla de an ́alisis, utilizando las reglas que generan
    directamente los sımbolos terminales, como sigue:
    Ni1 = {A | A → wi1 ∈ P }, 1 ≤ i ≤ n

2. Para j = 2, 3, . . . , n, hacer lo siguiente:
    − Para i = 1, 2, . . . , n − j + 1, hacer lo siguiente:
    − Inicializar Nij al conjunto vacıo.
    − Para k = 1, 2, . . . , j − 1, a ̃nadir a Nij todos los sımbolos no terminales A para los
        cuales A → BC ∈ P , con B ∈ Nik y C ∈ N(i+k)(j−k).

3. La cadena w pertenece a L(G) si y s ́olo si S ∈ N1n. *)
