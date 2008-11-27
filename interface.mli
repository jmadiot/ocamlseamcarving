(* Demande à l'utilisateur de modifier la matrice d'énergie *)
val get_filter : Graphics.image -> int array array

(* Demande à l'utilisateur la taille souhaitée pour la nouvelle image *)
val get_dimensions : (int*int*int) array array -> int*int

(* Attend que l'utilisateur appuie sur ESC puis rend la main *)
val wait_escape : unit -> unit
