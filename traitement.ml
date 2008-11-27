open Graphics;;

let image_of_matrix matrix = 
	let h = Array.length matrix
	and w = Array.length matrix.(0) in
	let mat = Array.make_matrix h w 0 in
	for i = 0 to (h-1) do
		for j = 0 to (w-1) do
			let (r,g,b) = matrix.(i).(j) in
			mat.(i).(j) <- rgb r g b;
		done;
	done;
	let image = make_image mat in
	image
;;

let rainbow q =
	let int, float = int_of_float, float_of_int in
	let n = 7 in
	let iq = int (q *. (float n)) in
	let rq = (float n) *. q -. (float iq) in
	let v = int (255.*.rq) in
	let w,t = 255-v, 255 in
	match iq with 
		| 0 -> rgb 0 0 v
		| 1 -> rgb 0 v t
		| 2 -> rgb 0 t w
		| 3 -> rgb v t 0
		| 4 -> rgb t w 0
		| 5 -> rgb t 0 v
		| 6 -> rgb t v t
		| 7 -> white
		| _ -> failwith "x%n < x ?";
;;

let rainbow_gray q =
	let int, float = int_of_float, float_of_int in
	let n = 7 in
	let iq = int (q *. (float n)) in
	let rq = (float n) *. q -. (float iq) in
	let o = 50 in
	let v = int (150.*.rq) in
	let w,t = 150-v, 150 in
	match iq with 
		| 0 -> rgb o o v
		| 1 -> rgb o v t
		| 2 -> rgb o t w
		| 3 -> rgb v t o
		| 4 -> rgb t w o
		| 5 -> rgb t o v
		| 6 -> rgb t v t
		| 7 -> rgb t t t
		| _ -> failwith "x%n < x ?";
;;



let dims m = (Array.length m.(0), Array.length m);;

let maxmatrix m =
 	let w,h = dims m in
	let ma = ref m.(0).(0) in
	for i = 0 to h-1 do
		for j =0 to w-1 do
			ma := max !ma m.(i).(j);
		done;
	done;
	!ma
;;

let make_rainbow m =
	let int, float = int_of_float, float_of_int in
	let w,h = dims m in
	let ma = maxmatrix m in
	let rb = Array.make_matrix h w 0 in
	for i = 0 to h-1 do
		for j =0 to w-1 do
			let x = m.(i).(j) in
			let q = (float x) /. (float ma) in
			rb.(i).(j) <- rainbow q;
		done;
	done;
	rb
;;


let make_rainbow_gray m =
	let int, float = int_of_float, float_of_int in
	let w,h = dims m in
	let ma = maxmatrix m in
	let rb = Array.make_matrix h w 0 in
	for i = 0 to h-1 do
		for j =0 to w-1 do
			let x = m.(i).(j) in
			let q = (float x) /. (float ma) in
			rb.(i).(j) <- rainbow_gray q;
		done;
	done;
	rb
;;


module Triplet = struct
	let sous triplet1  triplet2 =
		let a1,b1,c1 = triplet1 in
		let a2,b2,c2 = triplet2 in
		(a1-a2,b1-b2,c1-c2)

	let plus triplet1 triplet2 =
		let a1,b1,c1 = triplet1 in
		let a2,b2,c2 = triplet2 in
		(a1+a2,b1+b2,c1+c2)

	let carre triplet =
		let a, b, c = triplet in
		(a*a, b*b, c*c)

	let sqrt triplet =
		let a, b, c = triplet in
		let f x = int_of_float(sqrt(float_of_int(x))) in
		(f a, f b, f c)

	let somme triplet =
		let a,b,c = triplet in
		a+b+c

	let scalaire triplet d =
		let a,b,c = triplet in
		d*a,d*b,d*c
end

let sqrt_int n = int_of_float(sqrt(float_of_int n));;

let energy_matrix image = 
	let w,h = dims image in
	
	let f (x,y,z) (a,b,c) = sqrt_int (x*x+y*y+z*z+a*a+b*b+c*c) in
	
	let energie = Array.make_matrix h w (1) in
	
	let a = ref (0,0,0) and b = ref (0,0,0) in
	
	for i = 1 to h-2 do

		(* bord gauche*)
		b := Triplet.sous (image.(i-1).(0))  (image.(i+1).(0));
		a := Triplet.sous (image.(i).(0)) (image.(i).(1));
		energie.(i).(0) <- f !a !b;

		(*bord droit*)
		a := Triplet.sous (image.(i).(w-1)) (image.(i).(w-2));
		b := Triplet.sous (image.(i-1).(w-1)) (image.(i+1).(w-1));
		energie.(i).(w-1) <- f !a !b;

	done;

	for j = 1 to w-2 do

		(* bord haut *)
		b := Triplet.sous (image.(0).(j)) (image.(1).(j));
		a := Triplet.sous (image.(0).(j-1)) (image.(0).(j+1));
		energie.(0).(j) <- f !a !b;

		(* bord bas *)
		b := Triplet.sous (image.(h-1).(j)) (image.(h-2).(j));
		a := Triplet.sous (image.(h-1).(j-1)) (image.(h - 1).(j+1));
		energie.(h - 1).(j) <- f !a !b;

	done;

	(* et les 4 coins *)

	(*  en haut a gauche *)
	b := Triplet.sous (image.(0).(0)) (image.(1).(0));
	a := Triplet.sous (image.(0).(1)) (image.(0).(0));
	energie.(0).(0) <- f !a !b;

	(* en haut a droite *)
	b := Triplet.sous (image.(0).(w-1)) (image.(1).(w-1));
	a := Triplet.sous (image.(0).(w-1)) (image.(0).(w-2));
	energie.(0).(w-1) <-  f !a !b;

	(* en bas a droite *)
	b := Triplet.sous (image.(h-1).(w-1)) (image.(h-1).(w-2));
	a := Triplet.sous (image.(h-1).(w-1)) (image.(h-2).(w-1));
	energie.(h - 1).(w-1) <-  f !a !b;

	(* en bas a gauche *)
	b := Triplet.sous (image.(h-1).(1)) (image.(h-1).(0));
	a := Triplet.sous (image.(h-1).(0)) (image.(h-2).(0));
	energie.(h - 1).(0) <-  f !a !b;

	for i = 1 to h-2 do
		for j = 1 to w-2 do
			energie.(i).(j) <- f (Triplet.sous image.(i).(j-1) image.(i).(j+1)) (Triplet.sous image.(i-1).(j) image.(i+1).(j));
		done;
	done;
	
	energie
;;

let copy_matrix src =
	let w,h = dims src in
	let res = Array.make_matrix h w src.(0).(0) in
	for i = 0 to h-1 do
		for j = 0 to w-1 do
			res.(i).(j) <- src.(i).(j);
		done;
	done;
	res
;;

let min3 a b c = min (max a b) c;;
let indmin3 a b c = if a<b then if b<c then(0,a)else if a<c then(0,a)else(2,c)else if a<c then(1,b)else if b<c then(1,b)else(2,c);;
let indmin2 a b = if a<b then (0,a) else (1,b);;

let chemin_min energie =
	let w,h = dims energie in

	let chemins = Array.make_matrix h w 0 in
	let preds = Array.make_matrix h w (-42) in

	for i = 1 to h-1 do
		
		(*cas extremaux*)
		let ind,x = indmin2 chemins.(i-1).(0  ) chemins.(i-1).(1  ) in preds.(i).(0  ) <- ind;   chemins.(i).(0  ) <- energie.(i).(0  ) + x;
		let ind,x = indmin2 chemins.(i-1).(w-1) chemins.(i-1).(w-2) in preds.(i).(w-1) <- ind-1; chemins.(i).(w-1) <- energie.(i).(w-1) + x;
		
		(*cas generaux*)
		for j = 1 to w-2 do
			let ind,x = indmin3 chemins.(i-1).(j-1) chemins.(i-1).(j) chemins.(i-1).(j+1) in
			preds.(i).(j) <- ind-1;
			chemins.(i).(j) <- energie.(i).(j) + x;
		done;
	done;

	chemins, preds
;;


let build_chemin energie cost preds =
	let w,h = dims energie in
	let chemin = Array.make h (-1,-1) in
	let a=ref (h-1) and b = ref 0 in
	for j = 1 to w - 1 do
		if cost.(h-1).(!b) > cost.(h-1).(j) then b := j;
	done;
	chemin.(h - 1) <- (!a,!b);
	for i = h - 2 downto 0 do
		b := !b + preds.(!a).(!b);
		decr a;
		chemin.(i) <- (!a,!b);
	done;
	chemin
;;


let detruire_colonne matrice chemin =
  let w,h = dims matrice in
  let new_matrix = Array.make_matrix h (w-1) matrice.(0).(0) in
  let a = ref 0 in
    for i = 0 to h - 1 do
      a:=0;
      for j = 0 to w - 1 do
	if chemin.(i) <> (i,j) then
	  begin
	    new_matrix.(i).(!a) <- matrice.(i).(j);
	    a:= !a + 1;
	  end
      done;
    done;
new_matrix;;

let apply_filter energy filtre =
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

let rotate image = 
	let w,h = dims image in
	let image_rotate = Array.make_matrix w h image.(0).(0) in	
	for x = 0 to w-1 do
		for y = 0 to h-1 do
			image_rotate.(x).(y) <- image.(y).(x);
		done;
	done;
	image_rotate
;;

module SeamCarving = 
struct
	type t = {
		mutable pic : (int*int*int) array array;
		mutable energy : int array array;
		mutable cost : int array array;
		mutable preds : int array array;
		mutable returned : bool;
		mutable video : Graphics.image list
	}
	let init pic =
		let energy = energy_matrix pic in
		let cost, preds = chemin_min energy in
		{pic = pic; energy=energy; cost=cost; preds=preds; returned=false; video=[]}
	let shrink a =
		let seam = build_chemin a.energy a.cost a.preds in
		a.pic    <- detruire_colonne a.pic seam;
		a.energy <- detruire_colonne a.energy seam;		
		let cost, preds = chemin_min a.energy in
		a.cost   <- cost;
		a.preds  <- preds
		
	let get_energy a = (if a.returned then rotate else fun x->x) a.energy
	let get a = (if a.returned then rotate else fun x->x) a.pic
	let expand a n = ()
	let set_energy a e = a.energy <- e
	let shrinkhoriz seam n energypos =
		auto_synchronize false;
		for i = 1 to n do
			shrink seam;
			clear_graph();
			let im = image_of_matrix (get seam) in
			draw_image im 0 0;
			seam.video <- im :: seam.video;
			draw_image (make_image (make_rainbow (get_energy seam))) energypos 0;
			synchronize();
		done;
		auto_synchronize true
		
	let redim seam (redimx, redimy) =
		let image = get seam in
		let w,h = dims image in
		let reducx = if redimx<=0 then -redimx else failwith "On ne fait que réduire, désolé"
		and reducy = if redimy<=0 then -redimy else failwith "On ne fait que réduire, désolé" in
		resize_window (2*w+10) (h+10);
		shrinkhoriz seam reducx (w+10);
		if reducy > 0 then begin
			seam.pic <- rotate seam.pic;
			seam.energy <- rotate seam.energy;
			let cost, preds = chemin_min seam.energy in
			seam.cost <- cost;
			seam.preds <- preds;
			seam.returned <- true;
			shrinkhoriz seam reducy (w+10);
		end
	
	let replayrev a =
		let wait s = let t = Sys.time () in while Sys.time() < t+.s do () done in
		let pics = Array.of_list a.video in
		let n = Array.length pics in
		auto_synchronize false;
		for i=0 to n-1 do
			wait 0.01;
			clear_graph();
			draw_image pics.(i) 0 0;
			synchronize();
		done;
		auto_synchronize true
	
	let replay a =
		let wait s = let t = Sys.time () in while Sys.time() < t+.s do () done in
		let pics = Array.of_list a.video in
		let n = Array.length pics in
		auto_synchronize false;
		for i=n-1 downto 0 do
			wait 0.01;
			clear_graph();
			draw_image pics.(i) 0 0;
			synchronize();
		done;
		auto_synchronize true
		
end;;

