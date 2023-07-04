open! Base

let () =
  let open Cmdliner in
  Stdlib.exit @@ Cmd.eval Animated_julia.Cmd.cmd
;;
