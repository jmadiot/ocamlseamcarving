val reports : (string * float) array
val report : int -> string -> unit
val endreport : int -> unit
val rainbow : float -> Graphics.color
val rainbow_gray : float -> Graphics.color
val dims : 'a array array -> int * int
val maxmatrix : 'a array array -> 'a
val make_rainbow : int array array -> Graphics.color array array
val make_rainbow_gray : int array array -> Graphics.color array array
module type triple =
  sig
    val sous : int * int * int -> int * int * int -> int * int * int
    val plus : int * int * int -> int * int * int -> int * int * int
    val carre : int * int * int -> int * int * int
    val sqrt : int * int * int -> int * int * int
    val somme : int * int * int -> int
    val scalaire : int * int * int -> int -> int * int * int
  end
module Triplet :
  sig
    val sous : int * int * int -> int * int * int -> int * int * int
    val plus : int * int * int -> int * int * int -> int * int * int
    val carre : int * int * int -> int * int * int
    val sqrt : int * int * int -> int * int * int
    val somme : int * int * int -> int
    val scalaire : int * int * int -> int -> int * int * int
  end
val sqrt_int : int -> int
val energy_pixel : (int * int * int) array array -> int -> int -> int
val energy_matrix : (int * int * int) array array -> int array array
val copy_matrix : 'a array array -> 'a array array
val min3 : 'a -> 'a -> 'a -> 'a
val indmin3 : 'a -> 'a -> 'a -> int * 'a
val indmin2 : 'a -> 'a -> int * 'a
val chemin_min : int array array -> int array array * int array array
val build_chemin :
  'a array array -> 'b array array -> int array array -> (int * int) array
val detruire_colonne : 'a array array -> (int * int) array -> 'a array array
module type Seamcarving =
  sig
    type t
    val init : (int * int * int) array array -> t
    val shrink : t -> int -> unit
    val get : t -> (int * int * int) array array
    val expand : t -> int -> unit
  end
module Seam :
  sig
    type t = {
      mutable pic : (int * int * int) array array;
      mutable energy : int array array;
      mutable cost : int array array;
      mutable preds : int array array;
    }
    val init : (int * int * int) array array -> t
    val shrink : t -> 'a -> unit
    val get : t -> (int * int * int) array array
    val expand : 'a -> 'b -> unit
    val energy : t -> int array array
    val set_energy : t -> int array array -> unit
  end
