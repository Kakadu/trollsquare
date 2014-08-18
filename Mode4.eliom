(* View event there *)
{shared{
let container_classname = "view-event-detailed"
   }}

{client{
open Eliom_content
open Helpers
open Helpers_client
open Printf
open Firebug

let classes = [ "."^container_classname ]

let clear () =
  List.iter classes ~f:(fun cid -> ignore @@ Ojquery.(empty @@ jQelt @@ js_jQ cid) );
  ()

let _onModeChanged =
  let toggleMode on =
    if on then
      begin
        Lochash.set_mode Common.Mode4;
        List.iter JQ.Sel.show classes
      end else begin
        List.iter JQ.Sel.hide classes;
        clear ();
        Lochash.remove_value @@ Js.string "id";
      end
  in
  let f new_mode =
    toggleMode (new_mode = Common.Mode4);
    (*
    (lwt events = get_last_events () in
     let nodes = List.map ~f:make_node_for_event events in
     let parent = (Ojquery.jQelt @@ Ojquery.js_jQ ".main-events-list") in
     List.iter nodes ~f:(fun e -> JQ.append_element (To_dom.of_element e) parent);
     Lwt.return ()
    ) |>  Lwt.ignore_result; *)
    ()
  in
  React.E.map f Common.switch_mode_event

let draw_event (ev: Jstypes.dbevent_js Js.t) =
  let open Eliom_content.Html5.D in
  let parent = Ojquery.(jQelt @@ js_jQ ("."^container_classname) ) in
  let d = div [pcdata "WWWWWWWWWWWWQQQQQQQQQQQQQQQQQQQQQQQ"] in
  let (_: Ojquery.t) = JQ.append_element (Html5.To_dom.of_div d) parent in
  Lochash.set_value "id" (string_of_int ev##eventid);
  ()


let _onShowNode =
  let f (ev: Jstypes.dbevent_js Js.t option) =
    if Option.is_some ev then
    let ev = Option.get_exn ev in
    console##log (Js.string @@ sprintf "Showing event '%s'" (ev##title) );
    draw_event ev;
    ()
  in
  React.S.map f Common.show_node_event

(*
let do_show id =
  (*match Lochash.get_value "nodeid" with*)
  match Some id with
  | Some v -> show_node (Int64.of_string v)
  | None   -> console##log (Js.string "can't get node id")
 *)


}}
