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

let add_question_rpc
    : (int*string, int) Eliom_pervasives.server_function
  = server_function Json.t<int*string> (fun (parent_uid,text) -> Db.add_question ~parent_uid text)

}}

{shared{
let container_classname = "view-event-detailed"
   }}

{client{
open Eliom_content
open Eliom_content.Html5
open Helpers
open Helpers_client
open Printf
open Firebug


type options =
  { mutable event : Jstypes.dbevent_js Js.t
  }

let options = { event = Obj.magic () }

let classes = [ "."^container_classname ]

let clear () =
  console##log (Js.string "clear ()");
  List.iter classes ~f:(fun cid ->
                        let el = Ojquery.(jQelt @@ js_jQ cid) in
                        ignore @@ JQ.clear el ;
                       )

let interpretation_clicked i =
  print_endline @@ i##itext;
  Lwt.return ()

let question_clicked q =
  print_endline @@ q##qtext;
  Lwt.return ()

let event_clicked e =
  Lwt.return ()

let add_question_clicked _e = Dialogs.show "add-question-dialog"


let clear_questions_block () =
  ignore @@ JQ.clear @@ Ojquery.(jQelt @@ js_jQ ".mode4-questions-contatiner")

let draw_questions (qs: Jstypes.dbquestion_js Js.t Js.js_array Js.t) =
  let open Html5.D in
  let map_interpret i =
    let d = div ~a:[a_class ["mode4-interpret"]] [pcdata i##itext] in
    Lwt.ignore_result @@ Lwt_js_events.clicks (To_dom.of_div d) (fun _ _ -> interpretation_clicked i);
    d
  in
  let f (q: Jstypes.dbquestion_js Js.t) =
    let title_div = div ~a:[a_class ["mode4-question"]] [pcdata q##qtext] in
    Lwt.ignore_result @@ Lwt_js_events.clicks (To_dom.of_div title_div) (fun _ _ -> question_clicked q);
    div [ title_div
        ; div ~a:[a_class ["mode4-interpret-container"]] @@
            List.map ~f:map_interpret (Array.to_list @@ Js.to_array q##interprets)
        ]
  in
  let nodes = List.map ~f (Array.to_list @@ Js.to_array qs) in
  let add_question_btn = div ~a:[a_class ["mode4-add-question-btn"]; a_onclick add_question_clicked] [dummy_img ()] in
  let siblings = add_question_btn :: (nodes) in
  let d = div ~a:[a_class ["mode4-questions-container"]] siblings in
  let parent = Ojquery.(jQelt @@ js_jQ ("."^container_classname) ) in
  let (_: Ojquery.t) = JQ.append_element (Html5.To_dom.of_div d) parent in
  ()

let draw_event (ev: Jstypes.dbevent_js Js.t) =
  let open Eliom_content.Html5.D in
  let ts = ODates.(From.seconds ev##timestamp |> To.string Printer.default) in
  let title_div = div ~a:[a_class ["mode4-title"]]     [ pcdata ev##title] in
  Lwt.ignore_result @@ Lwt_js_events.clicks (To_dom.of_div title_div) (fun _ _ -> event_clicked ev);

  let d =
    div [ title_div
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
  let parent = Ojquery.(jQelt @@ js_jQ ("."^container_classname) ) in
  let (_: Ojquery.t) = JQ.append_element (Html5.To_dom.of_div d) parent in
  Lochash.set_value "uid" (string_of_int ev##uid);
  ()

let init_dialogs () =
  let () =
    let selector = "new_ques1tion_dialog" in
    let input_class = "new_question_dialog-input" in
    let open Eliom_content.Html5.D in
    let dialog_div =
      div [ pcdata "Enter quesiton below:"
          ; br()
          ; raw_input ~a:[a_class [input_class]] ~input_type:`Text ~value:"" ()
          ; br()
          ]
    in
    let onOK () =
      let text = JQ.val_ Ojquery.(jQelt @@ js_jQ ("."^input_class) ) in
      Dialogs.close selector;
      print_endline @@ sprintf "%s" text;
      clear_questions_block ();
      begin
        let event_uid = options.event##uid in
        lwt res = %add_question_rpc (event_uid, text) in
        lwt s2 = %get_questions_by_euid_rpc event_uid in
        console##log (Js.string s2);
        draw_questions (Json.unsafe_input @@ Js.string s2);
        Lwt.return ()
      end |> Lwt.ignore_result;
    in
    let onCancel () = Dialogs.close selector in
    let buttons = [ ("OK", onOK); ("Cancel", onCancel) ] in
    Dialogs.register ~buttons ~selector ~width:600 ~height:400
                     ~title:"New question..." ~content:[dialog_div];
    Dialogs.init ();
  in
  ()

let _onModeChanged =
  let toggleMode on =
    if on then
      begin
        if not (Dialogs.has_dialog "new-question-dialog") then init_dialogs ();
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
           let e = Json.unsafe_input @@ Js.string s in
           options.event <- e;
           draw_event e;
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
