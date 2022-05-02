module Rgb : sig
  type t = private
    { r : int
    ; g : int
    ; b : int
    }
end

val color : ?max_iter:int -> Complex.t -> Complex.t -> Rgb.t
val pixel_to_complex : width:int -> height:int -> int -> int -> Complex.t

val blit
  :  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> pitch:int
  -> c:Complex.t
  -> max_iter:int
  -> unit
