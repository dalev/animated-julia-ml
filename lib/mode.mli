type t =
  | Animate
  | Follow_mouse

val term : t Cmdliner.Term.t
