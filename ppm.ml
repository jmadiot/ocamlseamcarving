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

let make_matrix n l x =
	let m = Array.create n [||] in
	for i = 0 to n-1 do m.(i) <- Array.create l x; done; m
;;


type image =
	{
		palette : int;
		w : int;
		h : int;
		matrix : (int*int*int) array array
	}
;;

let dump im x y = 
	let h = Array.length im.matrix
	and w = Array.length im.matrix.(0) in
	let mat = Array.make_matrix h w 0 in
	for i = 0 to (h-1) do
		for j = 0 to (w-1) do
			let (r,g,b) = im.matrix.(i).(j) in
			mat.(i).(j) <- rgb r g b;
		done;
	done;
	let im = make_image mat in
		draw_image im x y;
;;



let file name =
	let _ = Sys.command (Printf.sprintf "convert %s %s.ppm" name name) in
	let im = open_in (name ^ ".ppm") in
	if(input_line im <> "P6") then (raise (Image_type_error "Format de ppm P6 nécessaire."));
	let (w, h) = get_dim (input_line im) in
	let p = get_palette (input_line im) in		
	let matrix = make_matrix h w (0,0,0) in
	for i = 0 to h-1 do
		for j = 0 to w-1 do
			let r = int_of_char (input_char im) in
			let g = int_of_char (input_char im) in
			let b = int_of_char (input_char im) in
			matrix.(i).(j) <- (r,g,b);
		done;
	done;
	let _ = Sys.command (Printf.sprintf "rm %s.ppm" name) in
	{palette=p;w=w;h=h;matrix=matrix}
;;

let matrix image = image.matrix;;

let im matrix = {palette=0;w=Array.length matrix.(0); h=Array.length matrix; matrix=matrix};;

let rgbimage_of_intimage fimage =
	let h=Array.length fimage
	and w=Array.length fimage.(0) in
	let image = Array.make_matrix h w 0 in
	for i = 0 to h-1 do
		for j = 0 to w-1 do
			let r,g,b = fimage.(i).(j) in
			image.(i).(j) <- rgb r g b;
		done;
	done;
	image
;;
