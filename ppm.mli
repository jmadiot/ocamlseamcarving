exception Image_type_error of string
val digit : char -> int
val get_dim : string -> int * int
val get_palette : string -> int
val make_matrix : int -> int -> 'a -> 'a array array
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
val rgbimage_of_intimage :
  (int * int * int) array array -> Graphics.color array array
