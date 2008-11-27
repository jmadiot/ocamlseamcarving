open Graphics;;
open Traitement;;


if Array.length Sys.argv = 1 or Sys.argv.(1) = "help" then begin
	(* Aide utilisateur *)
	print_endline (Printf.sprintf "Utilisation : %s image.jpg [sortie.jpg]" Sys.argv.(0));
end else begin
	open_graph " 800x600";
	set_window_title "Seam Carving";
	
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
	
	(* Si nom précisé, enregistrer dans un fichier *)
	let nom = if Array.length Sys.argv >= 3 then Sys.argv.(2) else "output.ppm" in
	Ppm.save nom (SeamCarving.get seam);
	
	(* fait joujou avec le résultat *)
	SeamCarving.replayrev seam;
	SeamCarving.replay seam;
	
	(* on attends "Echap" et on refait joujou *)
	Interface.wait_escape ();
	SeamCarving.replayrev seam;
	SeamCarving.replay seam
end;;
