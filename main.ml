open Graphics;;
open Traitement;;

open_graph " 800x600";;
set_window_title "Seam Carving";;

if Array.length (Sys.argv) <> 2 or Sys.argv.(1) = "help" then begin
	(* Aide utilisateur *)
	print_endline (Printf.sprintf "Utilisation : %s image.jpg" Sys.argv.(0));
end else begin
	let image = Ppm.file Sys.argv.(1) in
	
	(* Interface utilisateur : que faire ? *)
	let reductions = Interface.get_dimensions image in
	
	(* initialisation *)
	let seam = SeamCarving.init image in
	let energy = SeamCarving.get_energy seam in
	
	(* On crée une image décontrastée *)
	let fond = make_image (make_rainbow_gray energy) in
	
	(* Interface : l'utilisateur modifie sa fonction d'énergie comme il veut *)
	let filtre = Interface.get_filter fond in
	apply_filter energy filtre;
	
	(* réduction/agrandissement dans les deux sens de l'image *)
	SeamCarving.redim seam reductions;
	
	(* fait joujou avec le résultat *)
	SeamCarving.replayrev seam;
	SeamCarving.replay seam;
	
	(* on attends "Echap" et on refait joujou *)
	Interface.wait_escape ();
	SeamCarving.replayrev seam;
	SeamCarving.replay seam
end;;
