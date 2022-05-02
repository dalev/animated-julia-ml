module C = Complex
open! Base
module Complex = C
open Lwt.Syntax
module Sdl = Tsdl.Sdl

let ok' = function
  | Ok x -> x
  | Error (`Msg e) -> failwith e
;;

let log_err fmt =
  let open Caml in
  Lwt_fmt.eprintf (fmt ^^ "@.")
;;

module Event = struct
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

  let of_sdl_event e =
    match Sdl.Event.(enum (get e typ)) with
    | `Quit -> Quit
    | `Mouse_motion ->
      let x, y = Sdl.Event.(get e mouse_motion_x, get e mouse_motion_y) in
      Mouse_motion { x; y }
    | `Window_event ->
      begin
        match Sdl.Event.(window_event_enum @@ get e window_event_id) with
        | `Resized ->
          let g f = Int.of_int32_exn @@ Sdl.Event.get e f in
          let width, height = Sdl.Event.(g window_data1, g window_data2) in
          Window_resize { width; height }
        | _ -> Ignored
      end
    | _ -> Ignored
  ;;
end

module State : sig
  type t

  val create : unit -> t Or_error.t
  val out_of_date : t -> bool
  val set_up_to_date : t -> unit
  val handle_event : t -> Event.t -> unit Lwt.t
  val stopped : t -> unit Lwt.t
  val renderer : t -> Tsdl.Sdl.renderer
  val texture : t -> Tsdl.Sdl.texture
  val c : t -> Complex.t
  val destroy_sdl_resources : t -> unit
end = struct
  type t =
    { window : Sdl.window
    ; renderer : Sdl.renderer
    ; stop : unit Lwt.t
    ; stop_resolver : unit Lwt.u
    ; mutable out_of_date : bool
    ; mutable texture : Sdl.texture
    ; mutable c : Complex.t
    }

  let renderer t = t.renderer
  let texture t = t.texture
  let c t = t.c
  let stopped t = t.stop
  let out_of_date t = t.out_of_date
  let set_up_to_date t = t.out_of_date <- false

  let resize_texture t ~width ~height =
    let texture =
      ok'
      @@ Sdl.create_texture
           t.renderer
           Sdl.Pixel.format_rgb24
           Sdl.Texture.access_streaming
           ~w:width
           ~h:height
    in
    let old_texture = t.texture in
    t.texture <- texture;
    Sdl.destroy_texture old_texture
  ;;

  let handle_event t e =
    match (e : Event.t) with
    | Ignored -> Lwt.return_unit
    | Quit ->
      Lwt.wakeup t.stop_resolver ();
      Lwt.return_unit
    | Mouse_motion { x; y } ->
      let width, height = ok' @@ Sdl.get_renderer_output_size t.renderer in
      t.c <- Julia.pixel_to_complex ~width ~height x y;
      t.out_of_date <- true;
      Lwt.return_unit
    | Window_resize { width; height } ->
      resize_texture t ~width ~height;
      Lwt.return_unit
  ;;

  let create () =
    let stop, stop_resolver = Lwt.wait () in
    let w = 640
    and h = 480 in
    let flags = Sdl.Window.(shown + mouse_focus + resizable) in
    match Sdl.create_window ~w ~h "SDL events" flags with
    | Error (`Msg e) -> Or_error.error_s [%message "Create window" (e : string)]
    | Ok window ->
      let renderer =
        (* let flags = Sdl.Renderer.(presentvsync + accelerated) in *)
        let flags = Sdl.Renderer.accelerated in
        ok' @@ Sdl.create_renderer ~flags window
      in
      let texture =
        ok'
        @@ Sdl.create_texture
             renderer
             Sdl.Pixel.format_rgb24
             Sdl.Texture.access_streaming
             ~w
             ~h
      in
      Ok
        { window
        ; renderer
        ; stop
        ; stop_resolver
        ; texture
        ; c = Complex.zero
        ; out_of_date = true
        }
  ;;

  let destroy_sdl_resources t =
    Sdl.destroy_renderer t.renderer;
    Sdl.destroy_texture t.texture;
    Sdl.destroy_window t.window
  ;;
end

let event_loop handler =
  let e = Sdl.Event.create () in
  let rec loop () =
    if Sdl.poll_event (Some e)
    then begin
      match Sdl.Event.(enum (get e typ)) with
      | `Drop_file ->
        Sdl.Event.drop_file_free e;
        loop ()
      | _ ->
        let* () = handler e in
        loop ()
    end
    else
      let* () = Lwt_unix.sleep (1 // 100) in
      loop ()
  in
  Sdl.start_text_input ();
  loop ()
;;

let render_loop s ~max_iter =
  let stop = State.stopped s in
  let frame_counter = ref 0 in
  Lwt.async (fun () ->
      let rec frame_rate_loop () =
        let period = 3.0 in
        let* () = Lwt_unix.sleep period in
        let count = !frame_counter in
        frame_counter := 0;
        let rate = Float.of_int count /. period in
        let* () =
          Lwt_io.printlf "frame rate: %s" (Float.to_string_hum ~decimals:3 rate)
        in
        if Lwt.is_sleeping stop then frame_rate_loop () else Lwt.return_unit
      in
      frame_rate_loop ());
  let rps = [ 7.0; 13.0 ] in
  let angles t = List.map rps ~f:(fun rps -> t *. 2.0 *. Float.pi /. rps) in
  let _make_c t =
    fst
    @@ List.fold (angles t) ~init:(Complex.zero, 1.0) ~f:(fun (c, coeff) angle ->
           ( Complex.Infix.(c + Complex.(scale (polar ~radius:1.0 ~angle) coeff))
           , coeff *. 2.0 /. 3.0 ))
  in
  let _now =
    let t0 = Time_now.nanoseconds_since_unix_epoch () in
    fun () ->
      let ns = Int63.(Time_now.nanoseconds_since_unix_epoch () - t0) in
      Float.of_int63 ns *. 1e-9
  in
  let r = State.renderer s in
  let rec loop () =
    let t = State.texture s in
    match Sdl.lock_texture t None Bigarray.Int8_unsigned with
    | Ok (buf, pitch) ->
      let c = State.c s in
      if State.out_of_date s
      then begin
        State.set_up_to_date s;
        Julia.blit buf ~pitch ~c ~max_iter
      end;
      Sdl.unlock_texture t;
      ok' @@ Sdl.set_render_target r None;
      ok' @@ Sdl.set_render_draw_color r 0 0 0 0;
      ok' @@ Sdl.render_clear r;
      ok' @@ Sdl.render_copy r t;
      (* CR dalev: does render_present block or simply schedule the presentation for the next vsync? *)
      Sdl.render_present r;
      Int.incr frame_counter;
      let* () = Lwt.pause () in
      if Lwt.is_sleeping stop then loop () else Lwt.return_unit
    | Error (`Msg e) -> Lwt.fail_with @@ "lock_texture: " ^ e
  in
  loop ()
;;

let main' ~max_iter =
  let inits = Sdl.Init.(video + events) in
  match Sdl.init inits with
  | Error (`Msg e) -> log_err " SDL init: %s" e
  | Ok () ->
    (match State.create () with
    | Error e -> Lwt.fail (Error.to_exn e)
    | Ok state ->
      Lwt.async (fun () -> render_loop state ~max_iter);
      Lwt.async (fun () ->
          let+ () =
            event_loop @@ fun e -> State.handle_event state (Event.of_sdl_event e)
          in
          State.destroy_sdl_resources state;
          Sdl.quit ());
      State.stopped state)
;;

let main max_iter =
  let backend = Lwt_engine.Ev_backend.kqueue in
  let engine = new Lwt_engine.libev ~backend () in
  Lwt_engine.set engine;
  Lwt_main.run @@ main' ~max_iter
;;

let cmd =
  let open Cmdliner in
  let man = [ `S Manpage.s_description; `P "Draw a fractal" ] in
  let sdocs = Manpage.s_common_options in
  let docs = Manpage.s_options in
  let doc = "Draw a fractal" in
  let info = Cmd.info "animated_julia" ~doc ~sdocs ~man in
  let max_iter =
    let doc = "Max iterations to decide if point 'escapes' the Julia set" in
    Arg.(value & opt int 32 & info [ "max-iter" ] ~docs ~doc ~docv:"INT")
  in
  Cmd.v info Term.(const main $ max_iter)
;;