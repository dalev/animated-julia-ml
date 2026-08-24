type t

val make : first:Rgba.t -> last:Rgba.t -> stops:(float * Rgba.t) list -> size:int -> t
val find : t -> int -> float -> Rgba.t
