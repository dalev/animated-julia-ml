type t = private int

val make : r:int -> g:int -> b:int -> a:int -> int
val of_int : int -> t
val to_int : t -> int
val lerp : float -> t -> t -> t
