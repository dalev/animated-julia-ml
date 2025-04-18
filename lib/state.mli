open Base

type t

val is_running : t -> bool
val integrate : t -> dt:float -> unit
val make_handler : t -> (Event.t -> unit) Staged.t
val create_exn : pool:Domainslib.Task.pool -> no_vsync:bool -> mode:Mode.t -> unit -> t

val render
  :  t
  -> f:(Float_complex.t -> Base_bigstring.t -> int -> Domainslib.Task.pool -> 'a)
  -> unit

val reset_frame_count : t -> int
val destroy : t -> unit
