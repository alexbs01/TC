type 'a arbol_binario =
	Vacio
	| Nodo of 'a * 'a arbol_binario * 'a arbol_binario;;

(* t es el árbol del enunciado pero con dos nodos más colgando de 2*)
let t = Nodo(3, 
			 Nodo(2, 
				  Nodo(9, 
					   Nodo(10, Vacio, Vacio), Vacio), Vacio), 
			 Nodo(5, 
				  Nodo(4, Vacio, Vacio), 
				  Nodo(1, Vacio, Vacio)));;

(* inorder, preorder y postorder se hacen igual pero concatenando el nodo
   padre en el orden que corresponda *)				  
let rec in_orden = function
	| Vacio -> []
	| Nodo(x, hijoIzq, hijoDer) -> 
		(in_orden hijoIzq) @ (x::in_orden hijoDer);;(*izq @ padre @ der*)
	
let rec pre_orden = function
	| Vacio -> []
	| Nodo(x, hijoIzq, hijoDer) -> 
		(x)::(pre_orden hijoIzq @ pre_orden hijoDer);;(*padre @ izq @ der*)

let rec post_orden = function
	| Vacio -> []
	| Nodo(x, hijoIzq, hijoDer) -> 
		(post_orden hijoIzq) @ (post_orden hijoDer) @ ([x]);; (*izq @ der @ padre*)


let anchura arbol = let 
    rec aux arbolLista acc = match arbolLista with 
        | [] -> acc (* Retorno de lista*)
		| head::tail -> match head with
			| Vacio -> aux tail acc (* Cuando el arbol está vacío, retorna la lista *)
			| Nodo (v, hijoIzq, hijoDer) -> (* Si la primera posición del la lista 
											   es un arbol haz:*)
				aux (tail @ [hijoIzq] @ [hijoDer]) (acc @ [v])
    in aux [arbol] [];;
	
(* En la primera iteración tail está vacía, pero se concatenarán los hijos 
   izquierdo y derecho del primer. El nodo padre se concatena a acc que está vacía.
   
   En la segunda iteración, arbolLista tiene dos elementos, el nodo padre del primer
   elemento se concatena a acc, a la cola se le concatenan los hijos izquierdo y 
   derecho de la cabeza.
   
   En las siguientes iteraciones se guarda en acc la cabecera de arbolLista *)

in_orden t;; 	(* 10 9 2 3 4 5 1 *)
pre_orden t;; 	(* 3 2 9 10 5 4 1 *)
post_orden t;; 	(* 10 9 2 4 1 5 3 *)
anchura t;; 	(* 3 2 5 9 4 1 10 *)