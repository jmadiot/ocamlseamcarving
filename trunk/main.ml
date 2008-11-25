open Graphics;;
open Traitement;;

open_graph " 800x600";;
set_window_title "Seam Carving";;
 
let image = Ppm.file Sys.argv.(1);;

(* Interface utilisateur : que faire ? *)
let reductions = Interface.get_objectif image;;

(* initialisation *)
let seam = SeamCarving.init image;;
let energy = SeamCarving.get_energy seam;;

(* On crée une image décontrastée *)
let fond = make_image (make_rainbow_gray energy);;

(* Interface : l'utilisateur modifie sa fonction d'énergie comme il veut *)
let filtre = Interface.get_filter fond;;
apply_filter energy filtre;;

(* réduction/agrandissement dans les deux sens de l'image *)
SeamCarving.redim seam reductions;;

(* fait joujou avec le résultat *)
SeamCarving.replayrev seam;
SeamCarving.replay seam;;


Interface.wait_escape ();;
SeamCarving.replayrev seam;
SeamCarving.replay seam;;

(*

Remain to do :
- autre fonction d'énergie initiale

- vérifier compatibilité avec le sujet

- recalculer la fonction d'énergie

*)
