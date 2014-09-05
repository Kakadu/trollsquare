(* View event there *)
{server{

let get_event_by_uid_rpc
    : (int, string) Eliom_pervasives.server_function
  = server_function Json.t<int> Db.event_by_uid
let get_questions_by_euid_rpc
    : (int, string) Eliom_pervasives.server_function
  = server_function Json.t<int> Db.questions_by_event_uid

let get_interpret_conforms_rpc
    : (int, string) Eliom_pervasives.server_function
  = server_function Json.t<int> Db.interpret_info


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
  List.iter classes ~f:(fun cid -> (*
                        console##log (Js.string cid);
                        console##log (Ojquery.js_jQ cid); *)
                        let el = Ojquery.(jQelt @@ js_jQ cid) in
                        (*console##log (el);*)
                        ignore @@ JQ.clear el ;
                        (*console##log (el);*)
                       )
let draw_questions (qs: Jstypes.dbquestion_js Js.t Js.js_array Js.t) =
  let qs =
  let () = qs##reduceRight_init (fun _ _ _ _ -> (), ()) in
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
        List.iter JQ.Sel.hide classes;
        clear ();
        Lochash.remove_value @@ Js.string "uid";
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
           draw_event o;
           lwt s2 = %get_questions_by_euid_rpc id in
           console##log (Js.string s2);
           draw_questions (Json.unsafe_input @@ Js.string s2);
           Lwt.return ()
         end;
    ()
  in
  React.E.map f Common.switch_mode_event

let _onShowNode =
  let f (ev: Jstypes.dbevent_js Js.t option) =
    if Option.is_none ev then Lwt.return ()
    else
    let ev = Option.get_exn ev in
    draw_event ev;
    lwt s2 = %get_questions_by_euid_rpc ev##uid in
    draw_questions (Json.unsafe_input @@ Js.string s2);
    Lwt.return ()
  in
  React.S.map (fun o -> Lwt.ignore_result @@ f o) Common.show_node_event

(*
let do_show id =
  (*match Lochash.get_value "nodeid" with*)
  match Some id with
  | Some v -> show_node (Int64.of_string v)
  | None   -> console##log (Js.string "can't get node id")
 *)


}}
