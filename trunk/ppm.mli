exception Image_type_error of string

val get_image : (int*int*int) array array -> Graphics.image

val file : string -> (int*int*int) array array
