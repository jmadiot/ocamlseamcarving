(* affiche joliment une matrice d'énergie *)
val make_rainbow : int array array -> Graphics.color array array
val make_rainbow_gray : int array array -> Graphics.color array array

module SeamCarving :
sig
	type t
	
	(* Construit un objet de Seamcarving à l'aide d'une matrice de triplets RGB *)
	val init : (int * int * int) array array -> t
	
	(* retourne une matrice de triplets RGB *)
	val get : t -> (int * int * int) array array
	
	(* retourne la matrice d'énergie calculée *)
	val get_energy : t -> int array array
	
	(* redim image (a, b) enlève b colonnes puis a lignes *)
	val redim : t -> int*int -> unit
	
	(* affichage du seamcarving a posteriori, rapidement *)
	val replay : t -> unit
	val replayrev : t -> unit
end

(* applique le filtre à la matrice d'énergie *)
val apply_filter : int array array -> Graphics.color array array -> unit

(* retourne une image Graphics.image à l'aide d'une matrice R,G,B *)
val image_of_matrix : (int*int*int) array array -> Graphics.image
