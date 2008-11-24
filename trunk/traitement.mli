val dims : 'a array array -> int * int
val maxmatrix : 'a array array -> 'a

val make_rainbow : int array array -> Graphics.color array array
val make_rainbow_gray : int array array -> Graphics.color array array

module type Seamcarving =
  sig
    type t
    val init : (int * int * int) array array -> t
    val shrink : t -> unit
    val get : t -> (int * int * int) array array
    val expand : t -> int -> unit
    val energy : t -> int array array
    val redim : t -> int*int -> bool -> bool -> unit
  end

module Seam : Seamcarving

val apply_filter : int array array -> Graphics.color array array -> unit


