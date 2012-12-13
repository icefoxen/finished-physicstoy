(* main.ml
   Main game logic and setup.
 
   Simon Heath
   11/3/2005
*)

open Util;;
open Sdlvideo;;


let main () =
  (* Init... messing up the order of things, esp. config-files, could be bad
  *)
  Sdl.init [`EVERYTHING];
  Sdlwm.set_caption ~title: "Physics Toy!" ~icon: "None";
  Sdlmouse.show_cursor false;


  Sdlttf.init (); 
  Random.self_init ();


  (* Grafix setup *)
  let screen = set_video_mode ~w: !screenx ~h: !screeny ~bpp: 16
    [`DOUBLEBUF; `SWSURFACE] in
  

    logscreenx := 400.;
    logscreeny := 300.;
    (* Mainloop *)
  let game = new Driver.driver screen in
    game#mainLoop;

    (* De-init *)
    Sdlttf.quit ();
    Sdl.quit ()    
;;


let _ =
  main ();;

