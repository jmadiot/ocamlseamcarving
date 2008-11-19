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

let curseurnoir  = Array.map (Array.map (function -1 -> transp | _ -> 0)) curseurmat;;
let curseurblanc = Array.map (Array.map (function -1 -> transp | _ -> 16777215)) curseurmat;;


let superline (xa, ya) (xb, yb) coul points =
	let absi i = if i<0 then -i else i in
	let sign i = if i<0 then -1 else if i=0 then 0 else 1 in
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

let in_rect (x,y,w,h) a b = x<=a & a<x+w & y<=b & b<y+h;;







