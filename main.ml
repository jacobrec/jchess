open Io
open Jchess

let () = Printexc.record_backtrace true

let () =
  let g = Game.create () in 
  start_cmdline g
