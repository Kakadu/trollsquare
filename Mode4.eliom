(* View event there *)
{server{

let get_event_by_uid_rpc
    : (int, string) Eliom_pervasives.server_function
  = server_function Json.t<int> Db.event_by_uid

let get_questions_by_euid_rpc
    : (int, string) Eliom_pervasives.server_function
  = server_function Json.t<int> Db.questions_by_event_uid

let get_event_relations_rpc
    : (int, string) Eliom_pervasives.server_function
  = server_function Json.t<int> Db.event_relations

let get_interpret_conforms_rpc
    : (int, string) Eliom_pervasives.server_function
  = server_function Json.t<int> Db.interpret_info

let get_interpret_relations_rpc
    : (int, string) Eliom_pervasives.server_function
  = server_function Json.t<int> Db.interpret_relations

let add_question_rpc
    : (int*string, int) Eliom_pervasives.server_function
  = server_function Json.t<int*string> (fun (parent_uid,text) -> Db.add_question ~parent_uid text)

let remove_question_rpc
    : (int, unit) Eliom_pervasives.server_function
  = server_function Json.t<int> Db.remove_question

let add_interpret_rpc
    : (int*string, int) Eliom_pervasives.server_function
  = server_function Json.t<int*string> (fun (parent_uid,text) -> Db.add_interpret ~parent_uid text)

let remove_interpret_rpc
    : (int, unit) Eliom_pervasives.server_function
  = server_function Json.t<int> Db.remove_interpret

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

module Selectors = struct
  let new_interpret_dialog    = "new_interpret_dialog"
  let new_question_dialog     = "new_question_dialog"
  let confirmed_by_caption    = "mode4_confirmed_by_caption"
  let conflicts_with_caption  = "mode4_conflicts_with_caption"
  let confirmed_by_container  = "mode4_confirmed_by_container"
  let conflicts_with_container = "mode4_conflicts_with_container"
  let confirmed_by_item        = "mode4_confirmed_by_item"
  let conflicts_with_item      = "mode4_conflicts_with_item"
  let edit_conflicts_btn = "edit_conflicts_btn"
  let edit_conforms_btn = "edit_conforms_btn"
  module ConnDialog = struct
    let sel = "edit_connections_dialog"
    let search = sel^"--search"
    let search_container = search ^ "--container"
    let edit = sel ^ "--edit"
    let edit_container = edit ^ "--container"
  end
  let edit_connections_dialog = ConnDialog.sel
end

(* TODO: maybe get event id from url *)
type options =
  { mutable event : Jstypes.dbevent_js Js.t
  }

let options = { event = Obj.magic () }

let classes = [ "."^container_classname ]
let q_container_classname = "mode4-questions-container"

let clear () =
  console##log (Js.string "Mode4.clear ()");
  List.iter (Selectors.confirmed_by_container :: Selectors.conflicts_with_container :: classes)
            ~f:(fun cid -> ignore @@ JQ.clear Ojquery.(jQelt @@ js_jQ cid) )

let event_clicked e =
  let _ = Ojquery.(text_set @@ jQelt @@ js_jQ @@ ("."^Selectors.confirmed_by_caption))   "Event confirmed by:" in
  let _ = Ojquery.(text_set @@ jQelt @@ js_jQ @@ ("."^Selectors.conflicts_with_caption)) "Event conflicts with:" in
  lwt str = %get_event_relations_rpc options.event##uid in
  print_endline str;
  let _ = JQ.Sel.clear ("."^Selectors.confirmed_by_container) in
  Lwt.return ()

let interpretation_clicked i =
  let _ = Ojquery.(text_set @@ jQelt @@ js_jQ @@ ("."^Selectors.confirmed_by_caption))   "Interpretation confirmed by:" in
  let _ = Ojquery.(text_set @@ jQelt @@ js_jQ @@ ("."^Selectors.conflicts_with_caption)) "Interpretation conflicts with:" in
  lwt ansstr = %get_interpret_relations_rpc i##iuid in
  print_endline ansstr;
  let data_opt : Jstypes.conforms_conflicts Js.t Js.Optdef.t = Js.array_get (Json.unsafe_input @@ Js.string ansstr) 0 in
  let data = Js.Optdef.get data_opt (fun () -> failwith "can't parse data from server") in

  let _ = JQ.Sel.clear ("."^Selectors.confirmed_by_container) in
  let nodes =
    let xs: Jstypes.dbevent_js Js.t list = Array.to_list @@ Js.to_array data##conforms in
    let open Html5.D in
    let f ev =
      console##log_3 (ev, ev##title, Js.string ev##title );
      let ans = div ~a:[a_class [Selectors.confirmed_by_item]] [pcdata ev##title] in
      Lwt_js_events.clicks (To_dom.of_div ans) (fun _ _ -> print_endline @@ sprintf "clicked on conn-n %s" ev##title;
                                                           clear();
                                                           Common.show_node (Some ev);
                                                           Lwt.return ());
      ans
    in
    List.map xs ~f
  in
  let container = Ojquery.(jQelt @@ js_jQ @@ "."^Selectors.confirmed_by_container) in
  console##log (container);
  List.iter nodes ~f:(fun d -> ignore @@ JQ.append_element (Html5.To_dom.of_div d) container);
  Lwt.return ()


let add_question_clicked _e = Dialogs.show Selectors.new_question_dialog

let add_interpret_clicked quid _e =
  let _ = JQ.attr Ojquery.(jQelt @@ js_jQ @@  ("." ^Selectors.new_interpret_dialog)) "quid" (string_of_int quid) in
  Dialogs.show Selectors.new_interpret_dialog

let clear_questions_block () =
  ignore @@ JQ.hide @@ Ojquery.(jQelt @@ js_jQ ".node_n_question_container")

let rec remove_question_clicked quid _ev =
  Lwt.ignore_result (lwt _ = %remove_question_rpc quid in
                     let event_uid = options.event##uid in
                     lwt s2 = %get_questions_by_euid_rpc event_uid in
                     clear_questions_block ();
                     draw_questions (Json.unsafe_input @@ Js.string s2);
                     Lwt.return ())

and remove_interpret_clicked iuid _ev =
  lwt _ = %remove_interpret_rpc iuid in
  let event_uid = options.event##uid in
  lwt s2 = %get_questions_by_euid_rpc event_uid in
  clear_questions_block ();
  draw_questions (Json.unsafe_input @@ Js.string s2);
  Lwt.return ()

and draw_questions (qs: Jstypes.dbquestion_js Js.t Js.js_array Js.t) =
  let open Html5.D in
  let map_interpret i =
    let remove_btn = div ~a:[ a_class ["mode4-remove-interpret-btn"; "need_tooltip"]
                            ; a_onclick (fun e -> Lwt.ignore_result @@ remove_interpret_clicked i##iuid e)
                            ; a_title "Remove interpretation"
                            ]
                         [dummy_img ()]
    in
    let d = div ~a:[a_class ["mode4-interpret"]]
                [ pcdata i##itext
                ; remove_btn
                ]
    in
    Lwt.ignore_result @@ Lwt_js_events.clicks (To_dom.of_div d) (fun _ _ -> interpretation_clicked i);
    d
  in

  let f (q: Jstypes.dbquestion_js Js.t) =
    let title_div = div ~a:[a_class ["mode4-question"]] [pcdata q##qtext] in
    let remove_btn = div ~a:[ a_class ["mode4-remove-question-btn"; "need_tooltip"]
                            ; a_onclick (remove_question_clicked q##quid)
                            ; a_title "Remove question"
                            ]
                         [dummy_img ()]
    in
    let addi_btn = div ~a:[ a_class ["mode4-add-interpret-btn"; "need_tooltip"]
                          ; a_onclick (add_interpret_clicked q##quid)
                          ; a_title "Add interpretation"
                          ]
                       [dummy_img ()]
    in

    div ~a:[a_class ["node_n_question_container"]]
      [ title_div
      ; remove_btn
      ; addi_btn
      ; div ~a:[a_class ["mode4-interpret-container"]] @@
          List.map ~f:map_interpret (Array.to_list @@ Js.to_array q##interprets)
      ]
  in
  let nodes = List.map ~f (Array.to_list @@ Js.to_array qs) in
  let parent = Ojquery.(jQelt @@ js_jQ ("."^q_container_classname) ) in
  List.iter nodes ~f:(fun d -> ignore @@ JQ.append_element (Html5.To_dom.of_div d) parent);
  let need_tooltip = Ojquery.(jQelt @@ js_jQ (".need_tooltip") ) in
  JQ.tooltip need_tooltip;
  ()

let edit_conflict_button_clicked _ = Dialogs.show Selectors.edit_connections_dialog
let edit_confirms_button_clicked _ = Dialogs.show Selectors.edit_connections_dialog

let draw_event (ev: Jstypes.dbevent_js Js.t) =
  let open Eliom_content.Html5.D in
  let ts = ODates.(From.seconds ev##timestamp |> To.string Printer.default) in
  let title_div = div ~a:[a_class ["mode4-title"]]     [ pcdata ev##title] in
  Lwt.ignore_result @@ Lwt_js_events.clicks (To_dom.of_div title_div) (fun _ _ -> event_clicked ev);

  let d =
    let edit_conflicts_btn = div ~a:[ a_class ["mode4_" ^ Selectors.edit_conflicts_btn; "need_tooltip"]
                                    ; a_onclick (edit_conflict_button_clicked)
                                    ; a_title "Edit conflicts"
                                    ]
                                 [dummy_img ()]
    in
    let edit_confirms_btn  = div ~a:[ a_class ["mode4_" ^ Selectors.edit_conflicts_btn; "need_tooltip"]
                                    ; a_onclick (edit_confirms_button_clicked)
                                    ; a_title "Edit confirmations"
                                    ]
                                 [dummy_img ()]
    in

    div [ title_div
        ; div ~a:[a_class ["mode4-timestamp"]] [ pcdata ts ]
        ; div ~a:[a_class ["mode4-body"]] []
        ; div ~a:[a_class ["mode4-links"]]
              [ div ~a:[a_class ["mode4-confirmed-by"]]
                    [ div ~a:[a_class [Selectors.confirmed_by_caption]] [pcdata "Confirmed by"]
                    ; edit_confirms_btn
                    ; div ~a:[a_class [Selectors.confirmed_by_container]] []
                    ]
              ; div ~a:[a_class ["mode4-conflicts-with"]]
                    [ div ~a:[a_class [Selectors.conflicts_with_caption]] [pcdata "Conflicts with"]
                    ; edit_conflicts_btn
                    ; div ~a:[a_class [Selectors.conflicts_with_container]] []
                    ]
              ]
        ]
  in
  let parent = Ojquery.(jQelt @@ js_jQ ("."^container_classname) ) in
  let (_: Ojquery.t) = JQ.append_element (Html5.To_dom.of_div d) parent in

  let add_question_btn = div ~a:[ a_class ["mode4-add-question-btn"; "need_tooltip"]
                                ; a_onclick add_question_clicked
                                ; a_title "Add question..."
                                ]
                             [dummy_img ()]
  in
  let cnt = div ~a:[a_class [q_container_classname]] [add_question_btn] in
  let (_: Ojquery.t) = JQ.append_element (Html5.To_dom.of_div cnt) parent in
  Lochash.set_value "uid" (string_of_int ev##uid);
  ()

let init_edit_connections_dialog () =
  let selector = Selectors.ConnDialog.sel in
  if not(Dialogs.has_dialog selector) then
  let input_class = selector ^ "-input" in
  let open Eliom_content.Html5.D in
  let left_div =
    div [ div ~a:[a_class [Selectors.ConnDialog.search]]
              [ pcdata "Search there"
              (*; br() *)
              ; raw_input ~a:[a_class [input_class]] ~input_type:`Text ~value:"" ()
              ; div ~a:[a_class [Selectors.ConnDialog.edit_container]] []
              ]
        ; div ~a:[a_class [Selectors.ConnDialog.edit]]
              [ pcdata "current connections"
              ; div ~a:[a_class [Selectors.ConnDialog.edit_container]] []
              ]
        ]
  in
  let right_div = div [] in
  let onOK () =
    ()
  (*
    let jinput = Ojquery.(jQelt @@ js_jQ ("."^input_class) ) in
    let text = JQ.val_ jinput in
    JQ.set_val jinput "";
    Dialogs.close selector;
    clear_questions_block ();
    begin
      let event_uid = options.event##uid in
      lwt res = %add_question_rpc (event_uid, text) in
      lwt s2 = %get_questions_by_euid_rpc event_uid in
      draw_questions (Json.unsafe_input @@ Js.string s2);
      Lwt.return ()
    end |> Lwt.ignore_result; *)
  in
  let onClose () = Dialogs.close selector in
  let buttons = [ ("Close", onClose) ] in
  Dialogs.register ~buttons ~selector ~width:800 ~height:400
                   ~title:"Edit connections" ~content:[left_div; right_div];
  ()

let init_dialogs () =
  init_edit_connections_dialog();
  let () =
    let selector = Selectors.new_question_dialog in
    if not(Dialogs.has_dialog selector) then
    let input_class = selector ^ "-input" in
    let open Eliom_content.Html5.D in
    let dialog_div =
      div [ pcdata "Enter question below:"
          ; br()
          ; raw_input ~a:[a_class [input_class]] ~input_type:`Text ~value:"" ()
          ]
    in
    let onOK () =
      let jinput = Ojquery.(jQelt @@ js_jQ ("."^input_class) ) in
      let text = JQ.val_ jinput in
      JQ.set_val jinput "";
      Dialogs.close selector;
      clear_questions_block ();
      begin
        let event_uid = options.event##uid in
        lwt res = %add_question_rpc (event_uid, text) in
        lwt s2 = %get_questions_by_euid_rpc event_uid in
        draw_questions (Json.unsafe_input @@ Js.string s2);
        Lwt.return ()
      end |> Lwt.ignore_result;
    in
    let onCancel () = Dialogs.close selector in
    let buttons = [ ("OK", onOK); ("Cancel", onCancel) ] in
    Dialogs.register ~buttons ~selector ~width:600 ~height:400
                     ~title:"New question..." ~content:[dialog_div]
  in
  let () =
    let selector = Selectors.new_interpret_dialog in
    if not(Dialogs.has_dialog selector) then
    let input_class = selector ^ "_input" in
    let open Eliom_content.Html5.D in
    let dialog_div =
      div [ pcdata "Enter interpretation title below:"
          ; br()
          ; raw_input ~a:[a_class [input_class]] ~input_type:`Text ~value:"" ()
          ]
    in
    let onOK () =
      let jinput = Ojquery.(jQelt @@ js_jQ ("."^input_class) ) in
      let text = JQ.val_ jinput in
      JQ.set_val jinput "";
      let q_uid = int_of_string @@ Ojquery.(attr @@ jQelt @@ js_jQ ("."^selector)) "quid" in
      let event_uid = options.event##uid in
      Dialogs.close selector;
      clear_questions_block ();
      begin
        lwt res = %add_interpret_rpc (q_uid, text) in
        lwt s2  = %get_questions_by_euid_rpc event_uid in
        draw_questions (Json.unsafe_input @@ Js.string s2);
        Lwt.return ()
      end |> Lwt.ignore_result;
    in
    let onCancel () = Dialogs.close selector in
    let buttons = [ ("OK", onOK); ("Cancel", onCancel) ] in
    Dialogs.register ~buttons ~selector ~width:600 ~height:400
                     ~title:"New interpretation..." ~content:[dialog_div]
  in
  Dialogs.init ()

let _onModeChanged =
  let toggleMode on =
    if on then
      begin
        if not (Dialogs.has_dialog "new-question-dialog") then init_dialogs ();
        Lochash.set_mode Common.Mode4;
        List.iter JQ.Sel.show classes;
        init_dialogs ();
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
           console##log (e);
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


}}
