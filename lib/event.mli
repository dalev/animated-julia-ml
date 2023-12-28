open Base

type t =
  | Ignored
  | Quit
  | Mouse_motion of
      { x : int
      ; y : int
      }
  | Window_resize of
      { width : int
      ; height : int
      }
[@@deriving sexp_of]

val of_sdl_event : Tsdl.Sdl.event -> t
