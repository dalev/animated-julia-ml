open! Base
include module type of Caml.Complex

val of_real : float -> t
val ( = ) : t -> t -> bool
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
