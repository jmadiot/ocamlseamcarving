val curseurmat : Graphics.color array array
val curseurnoir : Graphics.color array array
val curseurblanc : Graphics.color array array
val absi : int -> int
val sign : int -> int
val superline :
  int -> int -> int -> int -> 'a -> (int * int * 'a) list ref -> unit
val build_button : string -> int -> int -> int * int * int * int
val in_rect : int * int * int * int -> int -> int -> bool
