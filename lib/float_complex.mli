open! Base
include module type of Stdlib.Complex

val of_real : float -> t

(** [sq t] = t * t, but computed slightly more efficiently *)
val sq : t -> t

val ( = ) : t -> t -> bool
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
