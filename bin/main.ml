open! Base

let () =
  let open Cmdliner in
  Caml.exit @@ Cmd.eval Animated_julia.Cmd.cmd
;;
