open Graphics;;
open Traitement;;

open_graph " 1600x600";;
 
let image = Ppm.file Sys.argv.(1);;

let seam = Seam.init image;;


(* CI-DESSOUS ON EDITE LA MATRICE D'ENERGIE CALCULEE CI-DESSUS. *)

let energy = Seam.energy seam;;

(* On crée une image décontrastée *)
let fond = make_image (make_rainbow_gray energy);;

(* Interface : l'utilisateur modifie sa fonction d'énergie comme il veut *)
let filtre = Interface.get_filter fond;;

(* mise à jour de la matrice d'énergie *)
let ma = maxmatrix energy in
let w,h = dims filtre in
for x=0 to w-1 do
	for y=0 to h-1 do
		let c = filtre.(y).(x) in 
		if c <> transp then
			energy.(y).(x) <- if c=0 then 0 else ma;
	done;
done;
;;

(* réduction horizontale de l'image *)
let w,h = dims image in
auto_synchronize false;
for i = 1 to w-5 do
	Seam.shrink seam 1;
	clear_graph();
	Ppm.dump (Seam.get seam) 0 0;
	draw_image (make_image (make_rainbow (Seam.energy seam))) (w+10) 0;
	synchronize();
done;
auto_synchronize true;;

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
