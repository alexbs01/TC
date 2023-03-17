# directory "../ocaml-talf/src/"
# load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;
	
	(* Estados; Símbolos; Estado inicial; Estados de aceptación; (Estado origen Estado destino Símbolo;)^+  *)
let af1 = "0 1 2 3; a b c; 0; 1 3; 0 1 a; 1 1 b; 1 2 a; 2 0 epsilon; 2 3 epsilon; 2 3 c;"