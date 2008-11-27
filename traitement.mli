val make_rainbow : int array array -> Graphics.color array array
val make_rainbow_gray : int array array -> Graphics.color array array

module SeamCarving :
sig
	type t
	val init : (int * int * int) array array -> t
	val get : t -> (int * int * int) array array
	val get_energy : t -> int array array
	val redim : t -> int*int -> unit
	val replay : t -> unit
	val replayrev : t -> unit
end

val apply_filter : int array array -> Graphics.color array array -> unit

val image_of_matrix : (int*int*int) array array -> Graphics.image
