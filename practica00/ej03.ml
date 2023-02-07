type 'a arbol_binario =
	Vacio
	| Nodo of 'a * 'a arbol_binario * 'a arbol_binario;;

let t = Nodo(3, 
			 Nodo(2, Nodo(9, Nodo(10, Vacio, Vacio), Vacio), Vacio), 
			 Nodo(5, 
				  Nodo(4, Vacio, Vacio), 
				  Nodo(1, Vacio, Vacio)));;
				  
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
    rec aux to_visit accumulative = match to_visit with 
        [] -> accumulative
    |   h::t -> match h with
            Vacio -> aux t accumulative
        |   Nodo (v, izq, der) -> aux (t @ [izq] @ [der]) (accumulative @ [v])
    in
    aux [arbol] [];;

in_orden t;;
pre_orden t;;
post_orden t;;
anchura t;; (*3 2 5 9 4 1 10*)