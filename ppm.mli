exception Image_type_error of string

(* Ouvre un fichier image d'un format quelconque et renvoie une matrice de triplet (r,g,b) *)
val file : string -> (int*int*int) array array
