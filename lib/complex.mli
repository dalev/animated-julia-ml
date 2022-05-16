type t =
  { re : float
  ; im : float
  }

val create : re:float -> im:float -> t
val polar : radius:float -> angle:float -> t
val zero : t
val dot : t -> t -> float
val quadrance : t -> float
val scale : t -> float -> t

module Infix : sig
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( = ) : t -> t -> bool
end
