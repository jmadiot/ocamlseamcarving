open Graphics;;

exception Image_type_error of string;;

let digit c = (int_of_char c) - (int_of_char '0');;

let get_dim s =
	let n = String.length s and i = ref 0 and w = ref 0 in
	while s.[!i] <> ' ' do w := 10 * !w + (digit s.[!i]); incr i; done;
	incr i;
	let h = ref 0 in
	while !i < n do h := 10 * !h + (digit s.[!i]); incr i; done;
	(!w, !h)
;;

let get_palette s =
	let n = String.length s and i = ref 0 and p = ref 0 in 
	while !i < n do p := 10 * !p + (digit s.[!i]); incr i; done; !p
;;


let file name =
	let _ = Sys.command (Printf.sprintf "convert %s %s.ppm" name name) in
	let im = open_in (name ^ ".ppm") in
	if(input_line im <> "P6") then (raise (Image_type_error "Format de ppm P6 nécessaire."));
	let (w, h) = get_dim (input_line im) in
	let _ = get_palette (input_line im) in		
	let matrix = Array.make_matrix h w (0,0,0) in
	for i = 0 to h-1 do
		for j = 0 to w-1 do
			let r = int_of_char (input_char im) in
			let g = int_of_char (input_char im) in
			let b = int_of_char (input_char im) in
			matrix.(i).(j) <- (r,g,b);
		done;
	done;
	let _ = Sys.command (Printf.sprintf "rm %s.ppm" name) in
	matrix
;;

