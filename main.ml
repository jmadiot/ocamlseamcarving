open Graphics;;
open Traitement;;

open_graph " 1600x600";;

let arg = Sys.argv.(1);;
let _ = Sys.command (Printf.sprintf "convert %s %s.ppm" arg arg);;
let image = Ppm.file (arg ^ ".ppm");;
let _ = Sys.command (Printf.sprintf "rm %s.ppm" arg) in ();;

let seam = Seam.init (Ppm.matrix image);;

let energy = Seam.energy seam;;
let ma = maxmatrix energy;;
let rb = make_rainbow_gray energy;;
let fond = make_image rb;;

let curseur = 
let b=rgb 255 255 255 in 
let t = transp in
make_image
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
let curseurblanc = 
let b=rgb 255 255 255 in 
let t = transp in
make_image
[|
[|t;t;b;b;b;t;t|];
[|t;b;b;b;b;b;t|];
[|b;b;b;b;b;b;b|];
[|b;b;b;b;b;b;b|];
[|b;b;b;b;b;b;b|];
[|t;b;b;b;b;b;t|];
[|t;t;b;b;b;t;t|];
|]
;;
let curseurnoir = 
let b=0 in 
let t = transp in
make_image
[|
[|t;t;b;b;b;t;t|];
[|t;b;b;b;b;b;t|];
[|b;b;b;b;b;b;b|];
[|b;b;b;b;b;b;b|];
[|b;b;b;b;b;b;b|];
[|t;b;b;b;b;b;t|];
[|t;t;b;b;b;t;t|];
|]
;;
let absi i = if i<0 then -i else i;;
let sign i = if i<0 then -1 else if i=0 then 0 else 1;;

let superline xa ya xb yb coul points =
	let dx, dy = absi (xb-xa), absi (yb-ya) in
	let sx, sy = sign (xb-xa), sign (yb-ya) in
	if dx > dy then begin
		for i=0 to dx/2 do
			let x=xa+2*i*sx in
			let y=ya+dy*(x-xa)*sx*sy/dx in
			points := (x,y,coul):: !points;
		done
	end else if dy >= dx & dy>0 then begin
		for i=0 to dy/2 do
			let y=ya+2*i*sy in
			let x=xa+dx*(y-ya)*sy*sx/dy in
			points := (x,y,coul):: !points;
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

let cw, ch = dims (dump_image curseur);;

let in_rect (x,y,w,h) a b = x<=a & a<x+w & y<=b & b<y+h;;

let curs c = if c=white then curseurblanc else curseurnoir;;

auto_synchronize false;
let points = ref [] in
let w,h = dims energy in
let last = ref (-1,-1) in
let button = ref false in
let editing = ref true in
let coul = ref white in
while !editing do
	let e = wait_next_event [Poll; Mouse_motion; Button_down] in
	clear_graph();
	let x,y = e.mouse_x, e.mouse_y in
	if e.button && x>=0 && x<w && y>=0 && y<h then begin
		let ax,ay = !last in
		moveto 100 560;
		draw_string (Printf.sprintf "xy %d,%d - %d,%d" ax ay x y);
		if !button then superline ax ay x y !coul points;
		last := (x,y);
		points := (x,y, !coul) :: !points;
	end;
	button := e.button;
	draw_image fond 0 0;
	draw_image curseur (e.mouse_x-cw/2) (e.mouse_y-ch/2);
	List.iter (fun (x,y,coul)->draw_image (curs coul) (x-cw/2) (y-ch/2);) (List.rev !points);
	set_color (if e.button then yellow else white); fill_rect 100 520 20 20;
	set_color black; draw_rect 100 520 20 20;
	set_color (if e.button && x>=0 && x<w && y>=0 && y<h then yellow else white); fill_rect 150 520 20 20;
	set_color black; draw_rect 150 520 20 20;
	let rect = build_button "SeamCarve-moi !" 300 520 in
	let blanc = build_button "Important" 250 570 in
	let noir = build_button "Pas important" 250 545 in
	if e.button & in_rect rect x y then editing := false;
	if e.button & in_rect noir x y then coul := black;
	if e.button & in_rect blanc x y then coul := white;
	synchronize();
done;
auto_synchronize true;
let curblanc = dump_image curseurblanc in
let curnoir = dump_image curseurnoir in
let putforce matrice x y v =
	let w,h=dims matrice in
	if 0<=x & x<w & 0<=y & y<h then	
		matrice.(y).(x) <- v;
in
let modify_energy (x,y,coul) =
	let cur = if coul=white then curblanc else curnoir in
	let cw, ch = dims cur in
	let _,h = dims energy in
	for i=0 to cw-1 do
		for j=0 to ch-1 do
			if cur.(j).(i) <> transp then
				putforce energy (x+i-cw/2) (h-(y+j-ch/2)) (if cur.(j).(i)=white then ma else 0);
		done;
	done;
in

List.iter modify_energy !points;

;;






let wait s = let t = Sys.time () in while Sys.time() < t+.s do () done;;

wait 2.;;

let w,h = dims (Seam.get seam) in
for i = 1 to w-5 do
	Seam.shrink seam 1;
	Ppm.dump (Ppm.im (Seam.get seam)) 0 0;
	let w,h = dims (Seam.get seam) in
	set_color white;moveto w 0;lineto w h;
done;;





let e = ref (wait_next_event [Poll]) in
while not !e.keypressed or !e.key <> (char_of_int 27) do
	e := wait_next_event [Button_down; Key_pressed];
done;;
