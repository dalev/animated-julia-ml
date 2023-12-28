open Base
open Eio.Std
module Sdl = Tsdl.Sdl
module Task = Domainslib.Task
module FArray = Stdlib.Float.ArrayLabels
module Complex = Float_complex
module Bigstring = Base_bigstring
module Write = Eio.Buf_write

let ( let+ ) m f =
  match m with
  | Ok x -> f x
  | Error (`Msg msg) -> Fmt.failwith "SDL failure: %s" msg
;;

let event_loop state =
  let e = Sdl.Event.create () in
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

let span_to_s span = Mtime.Span.to_float_ns span *. 1e-9

let render_loop s mono_clock ~max_iter =
  let now () = Eio.Time.Mono.now mono_clock in
  let dt = 1 // 100 in
  let last_time = ref @@ now () in
  let accum = ref 0.0 in
  while State.is_running s do
    let new_time = now () in
    let frame_time = Float.min (Mtime.span new_time !last_time |> span_to_s) 0.25 in
    accum := !accum +. frame_time;
    while Float.(!accum >= dt) do
      State.integrate s ~dt;
      accum := !accum -. dt
    done;
    State.render s ~f:(fun c buf width pool -> Julia.blit buf ~pool ~width ~c ~max_iter);
    last_time := new_time
  done
;;

let print f fmt = Fmt.pf f fmt

let frame_rate_loop state mono_clock writer =
  let period = Mtime.Span.(3 * s) in
  let now () = Eio.Time.Mono.now mono_clock in
  let print fmt = print writer fmt in
  print "frame rate loop started: period = %a@." Mtime.Span.pp period;
  let last_time = ref (now ()) in
  while State.is_running state do
    Eio.Time.Mono.sleep_span mono_clock period;
    let this_time = now () in
    let elapsed = Mtime.span this_time !last_time in
    let count = State.reset_frame_count state in
    let rate = Float.of_int count /. span_to_s elapsed in
    print "frame rate: %s@." (Float.to_string_hum ~decimals:3 rate);
    last_time := this_time
  done;
  print "frame rate loop stopped @."
;;

let main' ~pool ~max_iter ~no_vsync ~mode ~mono_clock ~writer =
  let+ () = Sdl.init Sdl.Init.(video + events) in
  let state = State.create_exn ~pool ~no_vsync ~mode () in
  print writer "fibers starting@.";
  Switch.run (fun sw ->
    Switch.on_release sw (fun () ->
      State.destroy state;
      Sdl.quit ());
    Fiber.fork_daemon ~sw (fun () ->
      (* this is a daemon so that we don't have to wait [period] seconds for the program to exit *)
      frame_rate_loop state mono_clock writer;
      `Stop_daemon);
    Fiber.fork ~sw (fun () -> render_loop state mono_clock ~max_iter);
    Fiber.fork ~sw (fun () -> event_loop state));
  print writer "switch shut down@."
;;

let main max_iter no_vsync mode =
  let num_domains = Stdlib.Domain.recommended_domain_count () - 1 in
  let pool = Task.setup_pool ~name:"compute-pool" ~num_domains () in
  Eio_main.run (fun env ->
    let mono_clock = Eio.Stdenv.mono_clock env in
    Write.with_flow (Eio.Stdenv.stdout env) (fun w ->
      let f =
        let out buf off len = Write.string w buf ~off ~len in
        let flush () = () in
        Stdlib.Format.make_formatter out flush
      in
      print f "#domains = %d@." num_domains;
      main' ~pool ~max_iter ~no_vsync ~mode ~mono_clock ~writer:f))
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
    Arg.(value & opt int 64 & info [ "max-iter" ] ~docs ~doc ~docv:"INT")
  in
  let no_vsync =
    let doc = "Disable vsync" in
    Arg.(value & flag & info [ "no-vsync" ] ~docs ~doc)
  in
  Cmd.v info Term.(const main $ max_iter $ no_vsync $ Mode.term)
;;
