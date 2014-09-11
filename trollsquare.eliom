{shared{
  open Eliom_lib
  open Eliom_content
  open Eliom_content.Html5
  open Html5.D
  open Helpers
}}

{client{
     open Printf
     open Jstypes
     open Helpers_client
     open Firebug
}}

module Trollsquare_app =
  Eliom_registration.App (
    struct
      let application_name = "trollsquare"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()


let initdb_rpc : (unit,unit) Eliom_pervasives.server_function
  = server_function Json.t<unit> Db.create_db

let generate_comments_div _ : Types.event list Lwt.t =
  lwt xs = Db.get_events 111. in
  Lwt.return []

let get_events_rpc
    : (Types.timestamp, Types.event list) Eliom_pervasives.server_function
  = server_function Json.t<Types.timestamp> generate_comments_div

let get_last_event_rpc
    : (Types.timestamp, string) Eliom_pervasives.server_function
  = server_function Json.t<Types.timestamp> Db.get_events

{client{
  let get_last_events () : dbevent_js Js.t list Lwt.t =
    (*let ts = ODate.Unix.now_milliseconds () |> int_of_float in*)
    let ts = (jsnew Js.date_now ())##valueOf () in
    lwt s = %get_last_event_rpc ts in
    Lwt.return (Array.to_list @@ Js.to_array @@ Json.unsafe_input @@ Js.string s)

}}

{client{

open Common
open Eliom_content.Html5

(*
let _onModeChanged =
  let toggleMode1 on =
    List.iter (if on then JQ.Sel.show else JQ.Sel.hide) [".main-event-view"; ".main-events-list"; ".main-right"]
  in
  let toggleMode2 on =
    if on then (TLView.show(); TLView.refresh ())
    else begin TLView.clear(); TLView.hide () end
  in
  let toggleMode3 on =
    List.iter (if on then JQ.Sel.show else JQ.Sel.hide) []
  in

  let f = function
    | Mode1 -> firelog "mode1"; toggleMode1 true;  toggleMode2 false; toggleMode3 false
    | Mode2 -> begin
               firelog "mode2"; toggleMode1 false; toggleMode2 true;  toggleMode3 false
             end
    | Mode3 -> firelog "mode3"; toggleMode1 false; toggleMode2 false; toggleMode3 true
  in
  React.E.map f switch_mode_event

let mode1_switch_callback on =
  List.iter (if on then JQ.Sel.show else JQ.Sel.hide) [".main-event-view"; ".main-events-list"; ".main-right"]
 *)

 }}


let main_handler () () =
  let initdb_btn = button ~button_type:`Button ~a:[a_class ["main-initdb-btn"]] [pcdata "Reinit server side"] in

  let heading = div ~a:[a_class ["main-heading"]]
                    [ pcdata "heading"
                    ; initdb_btn
                    ]
  in

  let left_area =
    let mode1_clicked = {Dom_html.mouseEvent Js.t->unit{ fun _ -> switch_mode Common.Mode1 }} in
    let mode2_clicked = {Dom_html.mouseEvent Js.t->unit{ fun _ -> switch_mode Common.Mode2 }} in
    let mode3_clicked = {Dom_html.mouseEvent Js.t->unit{ fun _ -> switch_mode Common.Mode3 }} in
    let mode4_clicked = {Dom_html.mouseEvent Js.t->unit{ fun _ -> switch_mode Common.Mode4 }} in

    ignore{unit{
               let open Helpers_client in
               let get_id = function
                 | Common.Mode1 -> "aux-button-mode1"
                 | Common.Mode2 -> "aux-button-mode2"
                 | Common.Mode3 -> "aux-button-mode3"
                 | Common.Mode4 -> "aux-button-mode4"
               in

               let f m =
                 console##log (Js.string "mode switched");
                 for i=1 to 4 do
                   Ojquery.(remove_attr (jQelt @@ js_jQ @@ sprintf "#aux-button-mode%d" i) "checked")
                   |> ignore
                 done;
                 let ok o =
                   print_endline "check element found";
                   let cb : Dom_html.element Js.t = (Js.Unsafe.coerce o) in
                   print_endline "setting attribute";
                   console##log (cb);
                   let _ = JQ.attr (Ojquery.jQelt o) "checked" "" in
                   (*cb##setAttribute (Js.string "checked", Js.string ""); *)
                   console##log (cb);
                   ()
                 in
                 let id = get_id m in
                 print_endline id;
                 with_element_by_id_exn (get_id m) ~ok
                                        ~bad:(fun _ -> console##log (Js.string "some error"))
               in
               f @@ Lochash.detect_mode ();
               let (_: 'a React.event) = React.E.map f Common.switch_mode_event in
               ()
          }};

    div ~a:[a_class ["main-left-bar"]]
        [ div ~a:[a_class ["main-chat-area"]] [pcdata "chat"]
        ; div ~a:[a_class ["main-aux-buttons-area"]]
              [ input ~a:[a_class ["aux-button-radio"]; a_id "aux-button-mode1"; a_name "aux-buttons"
                         ; Unsafe.string_attrib "checked" "" ]
                      ~input_type:`Radio ()
              ; Unsafe.node "label" ~a:[ Unsafe.string_attrib "for" "aux-button-mode1"
                                       ; a_class ["aux-button-label"]
                                       ; a_onclick mode1_clicked
                                       ] [pcdata "Mode 1 (Simple)"]
              ; input ~a:[a_class ["aux-button-radio"]; a_id "aux-button-mode2"; a_name "aux-buttons"]
                      ~input_type:`Radio ()
              ; Unsafe.node "label" ~a:[ Unsafe.string_attrib "for" "aux-button-mode2"
                                       ; a_class ["aux-button-label"]
                                       ; a_onclick mode2_clicked] [pcdata "Mode 2 (Timeline)"]
              ; input ~a:[ a_class ["aux-button-radio"]; a_id "aux-button-mode3"
                         ; a_name "aux-buttons" ]
                      ~input_type:`Radio ()
              ; Unsafe.node "label" ~a:[ Unsafe.string_attrib "for" "aux-button-mode3"
                                       ; a_class ["aux-button-label"]
                                       ; a_onclick mode3_clicked] [pcdata "Mode 3 (TODOs)"]
              ; input ~a:[ a_class ["aux-button-radio"]; a_id "aux-button-mode4"
                         ; a_name "aux-buttons" ]
                      ~input_type:`Radio ()
              ; Unsafe.node "label" ~a:[ Unsafe.string_attrib "for" "aux-button-mode4"
                                       ; a_class ["aux-button-label"]
                                       ; a_onclick mode4_clicked] [pcdata "Mode 4 (???)"]
              ; Unsafe.node "a" []
              ]
        ]
  in
  ignore {unit Lwt.t{
    let open Lwt_js_events in
    clicks (To_dom.of_button %initdb_btn) (fun e _ -> %initdb_rpc ())
  }};

  let events_list_div = div ~a:[a_class ["main-events-list"]] [] in
  let center_view =
    div ~a:[a_class ["main-center-view"]]
        [ div ~a:[a_class ["main-event-view"]] [pcdata "event description"]
        ; events_list_div
        ; div ~a:[a_class ["main-timeline"; "timeline-dual"]]
              [ div ~a:[a_class ["spine"]] []
              ]
        ]
  in
  let todo_view = div ~a:[a_class [Mode3.container_classname]] [] in
  let mode4_event_view = D.div ~a:[a_class [Mode4.container_classname] ] [] in
  let right_area =
    div ~a:[a_class ["main-right"]] [pcdata "right"]
  in

  ignore {unit{
              let params = Js.Unsafe.obj [||] in
              JQ.jq_selectable params Ojquery.(jQelt @@ js_jQ ".main-events-list")
         }};
  ignore {unit{
              switch_mode (Lochash.detect_mode ());
              let d = JQ.dialog ".test_dialog" "title"
                                [ ("OK", fun () -> print_endline "OK")
                                ; ("Cancel", fun () -> print_endline "Cancel")
                                ]
              in
              d##show ();
         }};

  let dialog_div = div ~a:[a_class ["test_dialog"]] [pcdata "dialog content" ] in
  Lwt.return [ heading; left_area; center_view; todo_view; right_area; mode4_event_view
               ; dialog_div ]


let () =
  Trollsquare_app.register
    ~service:main_service
    (fun () () ->
      lwt xs = main_handler () () in
      Lwt.return
        (Eliom_tools.F.html
           ~title:"trollsquare"
           ~js: [ ["js"; "jquery-1.10.2.min.js"]
                ; ["js"; "jquery-ui.1.10.4.js" ]
                ; ["js"; "jquery.timepicker.min.js" ]
                ]
           ~css:[ ["css"; "trollsquare.css"]
                ; ["css"; "main.css"]
                ; ["css"; "timeline.css"]
                ; ["css"; "todo-view.css"]
                ; ["css"; "full-event-view.css" ]  (* mode 4 *)
                ; ["css"; "work.css"]
                ; ["css"; "jquery-ui.css"] ]
           Html5.F.(body ~a:[] xs)
    ))
