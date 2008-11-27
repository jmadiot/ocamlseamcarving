exception Image_type_error of string

(* Ouvre un fichier image d'un format quelconque et renvoie une matrice de triplet (r,g,b) *)
val file : string -> (int*int*int) array array

(* Sauvegarde une image R,G,B dans une image de n'importe quel format *)
val save : string -> (int*int*int) array array -> unit
