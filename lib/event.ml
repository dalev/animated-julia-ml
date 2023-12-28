open Base
module Sdl = Tsdl.Sdl

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

let of_sdl_event e =
  match Sdl.Event.(enum (get e typ)) with
  | `Quit -> Quit
  | `Key_down ->
    let code = Sdl.Event.(get e keyboard_keycode) in
    if Sdl.K.q = code then Quit else Ignored
  | `Mouse_motion ->
    let x, y = Sdl.Event.(get e mouse_motion_x, get e mouse_motion_y) in
    Mouse_motion { x; y }
  | `Render_device_reset ->
    Fmt.epr "event: Render_device_reset@.";
    Ignored
  | `Window_event -> begin
    match Sdl.Event.(window_event_enum @@ get e window_event_id) with
    | `Size_changed ->
      let g f = Int.of_int32_exn @@ Sdl.Event.get e f in
      let width, height = Sdl.Event.(g window_data1, g window_data2) in
      Fmt.epr "window resize: %d, %d@." width height;
      Window_resize { width; height }
    | `Maximized ->
      Fmt.epr "window maximized@.";
      Ignored
    | `Unknown id ->
      Fmt.epr "window event unknown: %d@." id;
      Ignored
    | _ -> Ignored
  end
  | `Unknown id ->
    Fmt.epr "event type unknown: %d@." id;
    Ignored
  | _ -> Ignored
;;
