open Graphics;;
open Traitement;;

open_graph " 800x600";;
set_window_title "Seam Carving";;
 
let image = Ppm.file Sys.argv.(1);;

(* Interface utilisateur : que faire ? *)
let reductions = Interface.get_objectif image;;

(* initialisation *)
let seam = Seam.init image;;
let energy = Seam.energy seam;;

(* On crée une image décontrastée *)
let fond = make_image (make_rainbow_gray energy);;

(* Interface : l'utilisateur modifie sa fonction d'énergie comme il veut *)
let filtre = Interface.get_filter fond;;
apply_filter energy filtre;;


(* réduction/agrandissement dans les deux sens de l'image *)
Seam.shrink seam reductions true true;;

Interface.wait_escape ();;

(*

Remain to do :
- Gomme pour l'édition du filtre
- Commande du nombre de pixels à enlever,
- Et dans quelle direction

- autres fonctionnalité : mettre en évidence un élément
- autre fonction d'énergie initiale

- vérifier compatibilité avec le sujet

- recalculer la fonction d'énergie

*)
