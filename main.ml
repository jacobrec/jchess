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
      "Nh3";"a6";
      "Qd4";"a5";
      "e5";"a4";
      "e6";"Ra7";
      "e7";"Ra6";
    ] in

  let g = Game.create_from_moves test2 in 
  start_cmdline g
