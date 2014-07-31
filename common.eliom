{client{

type mode =
  | Mode1 (* тупой режим *)
  | Mode2 (* красивый*)
  | Mode3 (* режим TODO *)
  | Mode4 (* простомотри евента подробно *)

(*
type mode_switch_callback = bool -> unit
let cbs1 : mode_switch_callback list ref = ref []
let cbs2 : mode_switch_callback list ref = ref []
let cbs3 : mode_switch_callback list ref = ref []
let cbs4 : mode_switch_callback list ref = ref []
 *)

(*
let cur_mode = ref Mode4


let _map =  [(Mode1, cbs1); (Mode2, cbs2); (Mode3, cbs3); (Mode4, cbs4) ]
let set_mode m =
  if m <> !cur_mode
  then begin
      !(List.assoc !cur_mode _map) |> List.iter (fun f -> f false);
      !(List.assoc m _map) |> List.iter (fun f -> f true);
    end

let add_callback m ~cb =
  let xs = List.assoc m _map in
  xs := cb :: !xs
 *)

(* http://www.jqwidgets.com/jquery-ui-splitter/
   http://www.melonhtml5.com/demo/timeline/
*)
let (switch_mode_event, switch_mode) : (mode React.event * _) = React.E.create ()

   }}
