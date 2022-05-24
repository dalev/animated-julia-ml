val pixel_to_complex : width:int -> height:int -> int -> int -> Complex.t

val blit
  :  (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> pool:Domainslib.Task.pool
  -> pitch:int
  -> c:Complex.t
  -> max_iter:int
  -> unit
