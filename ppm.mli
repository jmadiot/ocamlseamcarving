exception Image_type_error of string

type image = (int*int*int) array array;;

val dump : image -> int -> int -> unit

val file : string -> image
