open Base
open Eio.Std
module Sdl = Tsdl.Sdl
module Task = Domainslib.Task
module FArray = Caml.Float.ArrayLabels
module Complex = Float_complex

module Sdl_result_syntax = struct
  let ( let+ ) m f =
    match m with
    | Ok x -> f x
    | Error (`Msg msg) -> failwith msg
  ;;
end

let log_err fmt =
  let open Caml in
  Format.eprintf (fmt ^^ "@.")
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
    | `Window_event -> begin
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

module Mode = struct
  type t =
    | Animate
    | Follow_mouse

  let term =
    let open Cmdliner in
    let doc = "Animate automatically or follow the mouse pointer" in
    let choices = [ "animate", Animate; "follow-mouse", Follow_mouse ] in
    let docv = String.concat ~sep:"|" (List.map ~f:fst choices) in
    Arg.(value & opt (enum choices) Animate & info [ "mode" ] ~docv ~doc)
  ;;
end

type pixels = (int32, Bigarray.int32_elt) Sdl.bigarray

module State : sig
  type t

  val create : pool:Task.pool -> no_vsync:bool -> mode:Mode.t -> unit -> t Or_error.t
  val is_running : t -> bool
  val make_handler : t -> (Event.t -> unit) Staged.t
  val destroy : t -> unit
  val integrate : t -> dt:float -> unit
  val render : t -> f:(Complex.t -> pixels -> int -> Task.pool -> unit) -> unit
  val reset_frame_count : t -> int
end = struct
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
    ; mutable mouse : Complex.t
    ; mutable frame_count : int
    }

  let is_running t = not @@ Promise.is_resolved t.stop
  let turns_per_sec = FArray.of_list [ 7.0; 13.0; 17.0 ]

  let radii =
    let base = 1.2
    and ratio = 2 // 3 in
    FArray.mapi turns_per_sec ~f:(fun i _ -> base *. Float.int_pow ratio i)
  ;;

  let make_c revolutions =
    let sum = ref Complex.zero in
    FArray.iter2 radii revolutions ~f:(fun radius turns ->
      let angle = 2.0 *. Float.pi *. turns in
      sum := Complex.(add !sum (polar radius angle)));
    !sum
  ;;

  let integrate t ~dt =
    let revs = t.revolutions in
    let f rev turn_per_sec =
      let rev = rev +. (dt /. turn_per_sec) in
      rev %. 1.0
    in
    let ( .%{} ) = FArray.get in
    let ( .%{}<- ) = FArray.set in
    for i = 0 to FArray.length revs - 1 do
      revs.%{i} <- f revs.%{i} turns_per_sec.%{i}
    done
  ;;

  let c t =
    match t.mode with
    | Animate -> Some (make_c t.revolutions)
    | Follow_mouse ->
      if t.out_of_date
      then begin
        t.out_of_date <- false;
        Some t.mouse
      end
      else None
  ;;

  let pixel_format = Sdl.Pixel.format_rgba8888

  let resize_texture t ~width ~height =
    let open Sdl_result_syntax in
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
          let open Sdl_result_syntax in
          let+ width, height = Sdl.get_renderer_output_size t.renderer in
          let c' = Julia.pixel_to_complex ~width ~height x y in
          if not Complex.(t.mouse = c')
          then begin
            t.mouse <- c';
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

  let create ~pool ~no_vsync ~mode () =
    let stop, stop_resolver = Promise.create () in
    let w = 640
    and h = 480 in
    let flags = Sdl.Window.(shown + mouse_focus + resizable) in
    match Sdl.create_window ~w ~h "Animated Julia Fractal" flags with
    | Error (`Msg e) -> Or_error.error_s [%message "Create window" (e : string)]
    | Ok window ->
      let or_error = function
        | Ok _ as ok -> ok
        | Error (`Msg e) -> Or_error.error_string e
      in
      let ( let* ) m f = Or_error.bind m ~f in
      let* renderer =
        let flags =
          if no_vsync
          then Sdl.Renderer.accelerated
          else Sdl.Renderer.(presentvsync + accelerated)
        in
        or_error @@ Sdl.create_renderer ~flags window
      in
      let* texture =
        or_error
        @@ Sdl.create_texture renderer pixel_format Sdl.Texture.access_streaming ~w ~h
      in
      Ok
        { window
        ; renderer
        ; stop
        ; stop_resolver
        ; texture
        ; mouse = Complex.zero
        ; revolutions = FArray.init (FArray.length turns_per_sec) ~f:(fun _ -> 0.0)
        ; out_of_date = true
        ; pool
        ; mode
        ; frame_count = 0
        }
  ;;

  let render t ~f =
    let tex = t.texture in
    Option.iter (c t) ~f:(fun c ->
      match Sdl.lock_texture tex None Bigarray.Int32 with
      | Ok (buf, pitch) ->
        f c buf pitch t.pool;
        Sdl.unlock_texture tex;
        let r = t.renderer in
        let open Sdl_result_syntax in
        let+ () = Sdl.render_copy r tex in
        Sdl.render_present r;
        t.frame_count <- 1 + t.frame_count
      | Error (`Msg e) -> failwith @@ "lock_texture: " ^ e)
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
end

let event_loop state =
  let e = Sdl.Event.create () in
  Sdl.start_text_input ();
  let handler = Staged.unstage @@ State.make_handler state in
  while State.is_running state do
    if Sdl.poll_event (Some e)
    then begin
      match Sdl.Event.(enum (get e typ)) with
      | `Drop_file -> Sdl.Event.drop_file_free e
      | _ ->
        let e = Event.of_sdl_event e in
        handler e
    end
    else Fiber.yield ()
  done
;;

let render_loop s clock ~max_iter =
  let now () = Eio.Time.now clock in
  let dt = 1 // 100 in
  let last_time = ref @@ now () in
  let accum = ref 0.0 in
  while State.is_running s do
    let new_time = now () in
    let frame_time =
      (* CR dalev: review this clamp *)
      Float.clamp_exn (new_time -. !last_time) ~min:dt ~max:(1 // 60)
    in
    accum := !accum +. frame_time;
    while Float.(!accum >= dt) do
      State.integrate s ~dt;
      accum := !accum -. dt
    done;
    State.render s ~f:(fun c buf pitch pool -> Julia.blit buf ~pool ~pitch ~c ~max_iter);
    last_time := new_time;
    Fiber.yield ()
  done
;;

let fork_frame_rate_loop ~sw state clock =
  (* this is a daemon so that we don't have to wait [period] seconds for the program to exit *)
  Fiber.fork_daemon ~sw (fun () ->
    let period = 3.0 in
    Fmt.pr
      "frame rate loop started: period = %s@."
      (Float.to_string_hum ~decimals:3 period);
    while State.is_running state do
      Eio.Time.sleep clock period;
      let count = State.reset_frame_count state in
      let rate = Float.of_int count /. period in
      Fmt.pr "frame rate: %s@." (Float.to_string_hum ~decimals:3 rate)
    done;
    Fmt.pr "frame rate loop stopped@.";
    `Stop_daemon)
;;

let main' ~pool ~max_iter ~no_vsync ~mode ~clock =
  let inits = Sdl.Init.(video + events) in
  match Sdl.init inits with
  | Error (`Msg e) -> log_err " SDL init: %s" e
  | Ok () -> begin
    match State.create ~pool ~no_vsync ~mode () with
    | Error e -> Error.raise e
    | Ok state ->
      Switch.run (fun sw ->
        let fork_all = List.iter ~f:(Fiber.fork ~sw) in
        Switch.on_release sw (fun () ->
          State.destroy state;
          Sdl.quit ());
        fork_frame_rate_loop ~sw state clock;
        fork_all
          [ (fun () -> render_loop state clock ~max_iter); (fun () -> event_loop state) ])
  end
;;

let main max_iter no_vsync mode =
  let pool =
    let num_domains = Caml.Domain.recommended_domain_count () - 1 in
    Task.setup_pool ~name:"compute-pool" ~num_domains ()
  in
  Eio_main.run (fun env ->
    let clock = Eio.Stdenv.clock env in
    main' ~pool ~max_iter ~no_vsync ~mode ~clock)
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
  let no_vsync =
    let doc = "Disable vsync" in
    Arg.(value & flag & info [ "no-vsync" ] ~docs ~doc)
  in
  Cmd.v info Term.(const main $ max_iter $ no_vsync $ Mode.term)
;;
