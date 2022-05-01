module C = Complex
open! Base
module Complex = C
open Lwt.Syntax

let ok' = function
  | Ok x -> x
  | Error (`Msg e) -> failwith e
;;

let log_err fmt =
  let open Caml in
  Lwt_fmt.eprintf (fmt ^^ "@.")
;;

let event_loop push_sdl_stream =
  let open Tsdl in
  let e = Sdl.Event.create () in
  let rec loop () =
    if Sdl.poll_event (Some e)
    then begin
      match Sdl.Event.(enum (get e typ)) with
      | `Quit ->
        push_sdl_stream None;
        Lwt.return_unit
      | `Drop_file ->
        Sdl.Event.drop_file_free e;
        loop ()
      | evt ->
        push_sdl_stream @@ Some evt;
        let* () = Lwt.pause () in
        loop ()
    end
    else
      let* () = Lwt_unix.sleep (1 // 100) in
      loop ()
  in
  Sdl.start_text_input ();
  loop ()
;;

let render_loop r t ~stop =
  let open Tsdl in
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
  let make_c t =
    fst
    @@ List.fold (angles t) ~init:(Complex.zero, 1.0) ~f:(fun (c, coeff) angle ->
           ( Complex.Infix.(c + Complex.(scale (polar ~radius:1.0 ~angle) coeff))
           , coeff *. 2.0 /. 3.0 ))
  in
  let now =
    let t0 = Time_now.nanoseconds_since_unix_epoch () in
    fun () ->
      let ns = Int63.(Time_now.nanoseconds_since_unix_epoch () - t0) in
      Float.of_int63 ns *. 1e-9
  in
  let rec loop () =
    match Sdl.lock_texture t None Bigarray.Int8_unsigned with
    | Ok (buf, pitch) ->
      let c = make_c @@ now () in
      Julia.blit buf ~pitch ~c;
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

let lwt_main sdl_stream renderer texture =
  let stream_done =
    Lwt_stream.iter_s (fun (_ : Tsdl.Sdl.Event.enum) -> Lwt.return_unit) sdl_stream
  in
  Lwt.async (fun () -> render_loop renderer texture ~stop:stream_done);
  stream_done
;;

let main' () =
  let open Tsdl in
  let inits = Sdl.Init.(video + events) in
  let sdl_stream, push_sdl_stream = Lwt_stream.create () in
  match Sdl.init inits with
  | Error (`Msg e) -> log_err " SDL init: %s" e
  | Ok () ->
    let flags = Sdl.Window.(shown + mouse_focus + resizable) in
    let w = 640
    and h = 480 in
    (match Sdl.create_window ~w ~h "SDL events" flags with
    | Error (`Msg e) -> log_err " Create window: %s" e
    | Ok window ->
      let r =
        let flags = Sdl.Renderer.(presentvsync + accelerated) in
        ok' @@ Sdl.create_renderer ~flags window
      in
      let t =
        ok'
        @@ Sdl.create_texture r Sdl.Pixel.format_rgb24 Sdl.Texture.access_streaming ~w ~h
      in
      Lwt.async (fun () ->
          let+ () = event_loop push_sdl_stream in
          Sdl.destroy_renderer r;
          Sdl.destroy_texture t;
          Sdl.destroy_window window;
          Sdl.quit ());
      lwt_main sdl_stream r t)
;;

let main () =
  let backend = Lwt_engine.Ev_backend.kqueue in
  let engine = new Lwt_engine.libev ~backend () in
  Lwt_engine.set engine;
  Lwt_main.run @@ main' ()
;;

let cmd =
  let open Cmdliner in
  let man = [ `S Manpage.s_description; `P "Draw a fractal" ] in
  let sdocs = Manpage.s_common_options in
  let doc = "Draw a fractal" in
  let info = Cmd.info "animated_julia" ~doc ~sdocs ~man in
  Cmd.v info Term.(const main $ Term.const ())
;;