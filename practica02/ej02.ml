# directory "../ocaml-talf/src/"
# load "talf.cma";;
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

let cadena1 = cadena_of_string "a b c";;

let cadena2 = cadena_of_string "b a";;

let cadena3 = cadena_of_string "a a";;