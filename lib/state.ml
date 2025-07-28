open Base
open Eio.Std
module Sdl = Tsdl.Sdl
module Task = Domainslib.Task
module FArray = Stdlib.Float.ArrayLabels
module Complex = Float_complex

let ( let+ ) m f =
  match m with
  | Ok x -> f x
  | Error (`Msg msg) -> Fmt.failwith "SDL failure: %s" msg
;;

type t =
  { window : Sdl.window
  ; renderer : Sdl.renderer
  ; stop : unit Promise.t
  ; stop_resolver : unit Promise.u
  ; pool : Task.pool
  ; mode : Mode.t
  ; revolutions : FArray.t
  ; mutable out_of_date : bool
  ; mutable texture : Sdl.texture
  ; mutable mouse : int * int
  ; mutable frame_count : int
  }

let is_running t = not @@ Promise.is_resolved t.stop

let turns_per_sec_inv =
  let f x = 1.0 /. x in
  FArray.of_list (List.map ~f [ 7.0; 13.0; 17.0 ])
;;

let radii =
  let base = 1.2
  and ratio = 2 // 3 in
  FArray.mapi turns_per_sec_inv ~f:(fun i _ -> base *. Float.int_pow ratio i)
;;

let make_c revolutions =
  let sum = ref Complex.zero in
  FArray.iter2 radii revolutions ~f:(fun radius turns ->
    let angle = 2.0 *. Float.pi *. turns in
    sum := Complex.(!sum + polar radius angle));
  !sum
;;

let integrate t ~dt =
  let revs = t.revolutions in
  let f rev turn_per_sec_inv =
    let rev = rev +. (dt *. turn_per_sec_inv) in
    rev %. 1.0
  in
  let ( .%{} ) = FArray.get in
  let ( .%{}<- ) = FArray.set in
  for i = 0 to FArray.length revs - 1 do
    revs.%{i} <- f revs.%{i} turns_per_sec_inv.%{i}
  done
;;

let c t =
  match t.mode with
  | Animate -> Some (make_c t.revolutions)
  | Follow_mouse ->
    if t.out_of_date
    then begin
      t.out_of_date <- false;
      let+ width, height = Sdl.get_renderer_output_size t.renderer in
      let x, y = t.mouse in
      let c = Julia.pixel_to_complex ~width ~height x y in
      Some c
    end
    else None
;;

let pixel_format = Sdl.Pixel.format_rgba8888

let resize_texture t ~width ~height =
  let+ texture =
    Sdl.create_texture
      t.renderer
      pixel_format
      Sdl.Texture.access_streaming
      ~w:width
      ~h:height
  in
  let old_texture = t.texture in
  t.texture <- texture;
  Sdl.destroy_texture old_texture
;;

let make_handler t =
  let handle_mouse_motion =
    match t.mode with
    | Animate -> fun ~x:_ ~y:_ -> ()
    | Follow_mouse ->
      fun ~x ~y ->
        let xy = x, y in
        if not ([%equal: int * int] t.mouse xy)
        then begin
          t.mouse <- xy;
          t.out_of_date <- true
        end
  in
  Staged.stage
  @@ fun e ->
  match (e : Event.t) with
  | Ignored -> ()
  | Quit -> Promise.resolve t.stop_resolver ()
  | Mouse_motion { x; y } -> handle_mouse_motion ~x ~y
  | Window_resize { width; height } -> resize_texture t ~width ~height
;;

let create_exn ~pool ~no_vsync ~mode () =
  let stop, stop_resolver = Promise.create () in
  let w = 800
  and h = 600 in
  let flags = Sdl.Window.(shown + mouse_focus + resizable + allow_highdpi) in
  let+ window = Sdl.create_window ~w ~h "Animated Julia Fractal" flags in
  let+ renderer =
    let flags =
      if no_vsync
      then Sdl.Renderer.accelerated
      else Sdl.Renderer.(presentvsync + accelerated)
    in
    Sdl.create_renderer ~flags window
  in
  let+ texture =
    Sdl.create_texture renderer pixel_format Sdl.Texture.access_streaming ~w ~h
  in
  { window
  ; renderer
  ; stop
  ; stop_resolver
  ; texture
  ; mouse = w / 2, h / 2
  ; revolutions = FArray.init (FArray.length turns_per_sec_inv) ~f:(fun _ -> 0.0)
  ; out_of_date = true
  ; pool
  ; mode
  ; frame_count = 0
  }
;;

let render t ~f =
  let tex = t.texture in
  Option.iter (c t) ~f:(fun c ->
    let p, set_p = Promise.create () in
    let (_ : unit Task.promise) =
      Task.async t.pool (fun () ->
        let+ buf, pitch = Sdl.lock_texture tex None Bigarray.Char in
        Exn.protect
          ~f:(fun () -> f c buf (pitch / 4) t.pool)
          ~finally:(fun () -> Sdl.unlock_texture tex);
        let r = t.renderer in
        let+ () = Sdl.set_render_draw_color r 0 0 0 0 in
        let+ () = Sdl.render_clear r in
        let+ () = Sdl.render_copy r tex in
        Sdl.render_present r;
        t.frame_count <- 1 + t.frame_count;
        Promise.resolve set_p (Ok ()))
    in
    Promise.await_exn p)
;;

let reset_frame_count t =
  let count = t.frame_count in
  t.frame_count <- 0;
  count
;;

let destroy t =
  Sdl.destroy_renderer t.renderer;
  Sdl.destroy_texture t.texture;
  Sdl.destroy_window t.window;
  Task.teardown_pool t.pool
;;
