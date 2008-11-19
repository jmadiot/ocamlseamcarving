open Graphics;;
open Traitement;;
open Interface;;

open_graph " 1600x600";;

let curseur      = make_image curseurmat;;
let curseurnoir  = make_image curseurnoir;;
let curseurblanc = make_image curseurblanc;;

let image = Ppm.file Sys.argv.(1);;

let seam = Seam.init (Ppm.matrix image);;

let energy = Seam.energy seam;;
let rb = make_rainbow_gray energy;;
let fond = make_image rb;;

let cw, ch = dims (dump_image curseur);;

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
	if e.button && in_rect (0,0,w,h) x y then begin
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
	let cond = e.button && x>=0 && x<w && y>=0 && y<h in
	let _  = build_button (if e.button then "S" else " ") 150 520 in
	let _  = build_button (if cond then "R" else " ") 170 520 in
	let rect  = build_button "SeamCarve-moi !" 300 520 in
	let blanc = build_button "Important"       250 570 in
	let noir  = build_button "Pas important"   250 545 in
	if e.button & in_rect rect  x y then editing := false;
	if e.button & in_rect noir  x y then coul := black;
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

let ma = maxmatrix energy in
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

List.iter modify_energy (List.rev !points);

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
