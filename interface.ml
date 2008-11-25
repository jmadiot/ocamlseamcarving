open Graphics;;
 
let curseurmat =
	let b=rgb 255 255 255 in 
	let t = transp in
	[|
		[|t;t;0;0;0;t;t|];
		[|t;0;b;b;b;0;t|];
		[|0;b;b;b;b;b;0|];
		[|0;b;b;b;b;b;0|];
		[|0;b;b;b;b;b;0|];
		[|t;0;b;b;b;0;t|];
		[|t;t;0;0;0;t;t|];
	|]
;;

let curseurnoirmat  = Array.map (Array.map (function -1 -> transp | _ -> 0)) curseurmat	;;
let curseurblancmat = Array.map (Array.map (function -1 -> transp | _ -> 16777215)) curseurmat;;

let superline (xa, ya) (xb, yb) coul add =
	let absi i = if i<0 then -i else i in
	let sign i = if i<0 then -1 else if i=0 then 0 else 1 in
	let dx, dy = absi (xb-xa), absi (yb-ya) in
	let sx, sy = sign (xb-xa), sign (yb-ya) in
	if dx > dy then begin
		for i=0 to dx/2 do
			let x=xa+2*i*sx in
			let y=ya+dy*(x-xa)*sx*sy/dx in
			add x y coul;
		done
	end else if dy >= dx & dy>0 then begin
		for i=0 to dy/2 do
			let y=ya+2*i*sy in
			let x=xa+dx*(y-ya)*sy*sx/dy in
			add x y coul;
		done
	end;
;;


let build_button text x y =
	let w,h = text_size text in
	set_color (rgb 227 227 227); fill_rect (x+1) (y+1) (w+1) h;
	set_color black; draw_rect x y (w+3) (h+2);
	moveto (x+2) (y+1);
	draw_string text;
	x,y,(w+3),(h+2)
;;

let in_rect (x,y,w,h) a b = x<=a & a<x+w & y<=b & b<y+h;;


let dims m = (Array.length m.(0), Array.length m);;



let copy_matrix_into cible source x y =
	let cw, ch = dims source
	and w, h = dims cible in
	for i = max x 0 to min (w-1) (x+cw-1) do
		for j = max y 0 to min (h-1) (y+ch-1) do
			let new_pixel = source.(j-y).(i-x) in
			if new_pixel <> transp then
				cible.(j).(i) <- new_pixel;
		done;
	done;
;;


let get_filter fond =
	let curseur      = make_image curseurmat
	and curseurnoir  = make_image curseurnoirmat in

	let cw, ch = dims curseurmat in
	let w,h = dims (dump_image fond) in
	
	resize_window (w+50) (h+50);
	
	auto_synchronize false;
	
	let filtre = Array.make_matrix h w transp in
	
	let add x y important = copy_matrix_into filtre (if important then curseurblancmat else curseurnoirmat) (x-cw/2) (h-1-y-ch/2) in

	let last = ref (-1,-1) in
	let clicking = ref false in
	let editing = ref true in
	let important = ref true in
	while !editing do
		let e = wait_next_event [Poll; Mouse_motion; Button_down] in
		
		clear_graph();
		
		let x,y = e.mouse_x, e.mouse_y in
	
		if e.button then begin
			if !clicking then superline !last (x, y) !important add;
			add x y !important;
			last := (x,y);
		end;
	
		draw_image fond 0 0;
	
		draw_image (make_image filtre) 0 0;
		
		let rect  = build_button "Resize"    300 (h+10) in
		let blanc = build_button "Conserver" 10 (h+30) in
		let noir  = build_button "Enlever"   10 (h+10) in
	
		if e.button & not !clicking & in_rect rect  x y then editing := false;
		if e.button & not !clicking & in_rect noir  x y then important := false;
		if e.button & not !clicking & in_rect blanc x y then important := true;

		clicking := e.button;
	
		if not (in_rect rect x y or in_rect blanc x y or in_rect noir x y) then 
			draw_image (if !important then curseur else curseurnoir) (e.mouse_x-cw/2) (e.mouse_y-ch/2);

		synchronize();
	done;
	clear_graph();
	auto_synchronize true;
	filtre
;;

let blanchir = Array.map (Array.map (fun (a,b,c)->a/2+128,b/2+128,c/2+128));;

let get_objectif image =
	let w,h = dims image in
	resize_window (2*w+10) (2*h+10);
	draw_image (Traitement.get_image image) 0 0;
	let nw, nh = ref w, ref h in
	let editing = ref true in
	auto_synchronize false;
	while !editing do
		let e = wait_next_event [Poll; Mouse_motion; Button_down] in
		clear_graph();
		if e.button then editing := false else begin
			nw := min (max 0 (e.mouse_x+1)) w;
			nh := min (max 0 (e.mouse_y+1)) h;
			set_color (rgb 220 220 255);
			fill_rect 0 0 !nw !nh;
			let im = Traitement.get_image image in
			draw_image im 0 0;
			for x=0 to !nw-1 do
				for y=0 to !nh-1 do
					if in_rect (0,0,w,h) x y then begin
						let r,g,b = image.(h-1-y).(x) in
						set_color (rgb (r/2+128) (g/2+128) (b/2+128));
						plot x y;
					end;
				done;
			done;
		end;
		synchronize();
	done;
	auto_synchronize true;
	!nw - w, !nh - h
;;

let wait s = let t = Sys.time () in while Sys.time() < t+.s do () done;;


let wait_escape () =
	let e = ref (wait_next_event [Poll]) in
	while not !e.keypressed or !e.key <> (char_of_int 27) do
		e := wait_next_event [Button_down; Key_pressed];
	done
;;









