(* View event there *)
{server{

let get_event_by_uid_rpc
    : (int, string) Eliom_pervasives.server_function
  = server_function Json.t<int> Db.event_by_uid


}}

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
  console##log (Js.string "clear ()");
  List.iter classes ~f:(fun cid ->
                        console##log (Js.string cid);
                        ignore @@ JQ.clear @@ Ojquery.(jQelt @@ js_jQ cid) );
  ()

let draw_event (ev: Jstypes.dbevent_js Js.t) =
  let open Eliom_content.Html5.D in
  let parent = Ojquery.(jQelt @@ js_jQ ("."^container_classname) ) in
  let ts = ODates.(From.seconds ev##timestamp |> To.string Printer.default) in
  let d =
    div [ div ~a:[a_class ["mode4-title"]]     [ pcdata ev##title]
        ; div ~a:[a_class ["mode4-timestamp"]] [ pcdata ts ]
        ; div ~a:[a_class ["mode4-body"]] []
        ; div ~a:[a_class ["mode4-links"]]
              [ div ~a:[a_class ["mode4-confirmed-by"]]
                    [ div [pcdata "Confirmed by"]
                    ; div ~a:[a_class ["mode4-confirmed-by-container"]] []
                    ]
              ; div ~a:[a_class ["mode4-conflicts-with"]]
                    [ div [pcdata "Conflics with"]
                    ; div ~a:[a_class ["mode4-conflicts-with-container"]] []
                    ]
              ]
        ]
  in
  let (_: Ojquery.t) = JQ.append_element (Html5.To_dom.of_div d) parent in
  Lochash.set_value "uid" (string_of_int ev##uid);
  ()

let _onModeChanged =
  let toggleMode on =
    if on then
      begin
        Lochash.set_mode Common.Mode4;
        List.iter JQ.Sel.show classes
      end else begin
        console##log (Js.string "turn Mode4 off");
        List.iter JQ.Sel.hide classes;
        clear ();
        Lochash.remove_value @@ Js.string "id";
      end
  in
  let f new_mode =
    toggleMode (new_mode = Common.Mode4);
    match Lochash.get_value "uid" with
    | None -> ()
    | Some id -> (* id is inner node id `uid`, not neo4j id *)
       let id = int_of_string @@ Js.to_string id in
       Lwt.ignore_result begin
           lwt s = %get_event_by_uid_rpc id in
           console##log (Js.string s);
           let o = Json.unsafe_input @@ Js.string s in
           console##log (o);
           draw_event o;
           Lwt.return ()
         end;
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
