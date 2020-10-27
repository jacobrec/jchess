open Io
open Jchess

let () = Printexc.record_backtrace true

let () =
  let test2 = [
      "e4";"e5";
      "Bc4";"f5";
      "Qh5";"g6";
      "Qg6";"Ke7";
      "Qf5";"Ke8";
      "Qe5";"Qe7";
      "Bf7";"Kd8";
      "a3";"Qh4";
      "Nh3";"a6"] in
  let _test1 = ["e4"; "e5"; "Bc4"; "f5"; "Qh5"] in (* Black Check *)

  let g = Game.create_from_moves test2 in 
  start_cmdline g
