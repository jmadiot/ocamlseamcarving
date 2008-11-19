exception Image_type_error of string

type image = {
  palette : int;
  w : int;
  h : int;
  matrix : (int * int * int) array array;
}

val dump : image -> int -> int -> unit

val file : string -> image

val matrix : image -> (int * int * int) array array

val im : (int * int * int) array array -> image
