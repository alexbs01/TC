let rec mapdoble f1 f2 l = match l with
	| h1::h2::t -> (f1 h1)::(f2 h2)::(mapdoble f1 f2 t)
	| _::[]
	| [] -> l;;
	
(* val mapdoble: ('a -> 'a) -> ('a -> 'a) -> 'a list -> 'a list = <fun>
 *)

(* mapdoble (function x -> x*2) (function x -> "x") [1;2;3;4;5];;

Debería dar un error porque las listas solo pueden ser de un único 
tipo, por lo que no se pueden mezclar enteros y strings en una misma
lista

Salida de esa línea:
Error: This expression has type string but an expression was expected
of type int

*)

(* let y = function x -> 5 in mapdoble y;; 

- : (int -> int) -> int list -> int list = <fun>

*)
