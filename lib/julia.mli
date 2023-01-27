open! Base

val pixel_to_complex : width:int -> height:int -> int -> int -> Float_complex.t

val blit
  :  Base_bigstring.t
  -> pool:Domainslib.Task.pool
  -> width:int
  -> c:Float_complex.t
  -> max_iter:int
  -> unit
