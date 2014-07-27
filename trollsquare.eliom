{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
}}

{client{
     open Eliom_content.Html5
     open Printf
     open Helpers_client
     open Jstypes
}}
open Helpers

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
  lwt xs = Db.get_events 111 in
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
    lwt s = %get_last_event_rpc (int_of_float ts) in
    Lwt.return (Array.to_list @@ Js.to_array @@ Json.unsafe_input @@ Js.string s)

}}

{client{

open Eliom_content.Html5

type mode =
  | Mode1 (* тупой режим *)
  | Mode2 (* красивый*)
  | Mode3 (* режим TODO *)

(* http://www.jqwidgets.com/jquery-ui-splitter/
   http://www.melonhtml5.com/demo/timeline/
*)
let switch_mode_event, switch_mode = React.E.create ()

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



(*
let split s =
    let len = String.length s in
    let rec aux acc = function
      | 0 -> acc
      | n -> aux (s.[n - 1] :: acc) (pred n)
    in aux [] len
       *)
(*
let value_signal, set_value = React.S.create "initial"
(* value_signal : string React.signal *)

let value_len = React.S.map String.length value_signal
 *)
(* value_len : int React.signal *)
(*
let content_signal : Html5_types.div_content_fun elt React.signal =
  React.S.map (fun value ->  let l = split value in
                F.div (
                  List.map (fun c ->
                      F.p [F.pcdata (Printf.sprintf "%c" c) ]
                    ) l
                )
              ) value_signal
  *)
(*
let html_value_signal : [ `PCDATA ] R.elt list React.signal
  = React.S.map (fun s -> [pcdata s]) value_signal

let make_color len =
  let d = (len * 10) mod 255 in
  Printf.sprintf "color: rgb(%d,%d,%d)" d d d
  *)

let make_node_for_event e =
  let ts = ODates.(From.seconds e##timestamp |> To.string Printer.default) in
  li ~a:[a_class ["event-list-item"]]
    [ div ~a:[a_class ["event-list-item-title"]] [pcdata @@ Js.to_string e##title]
    ; div ~a:[a_class ["event-list-item-time"]] [pcdata ts]
  ]


 }}


let main_handler () () =
  let initdb_btn = button ~button_type:`Button ~a:[a_class ["main-initdb-btn"]] [pcdata "Reinit server side"] in

  let heading = div ~a:[a_class ["main-heading"]]
                    [ pcdata "heading"
                    ; initdb_btn
                    ]
  in

  let left_area =
    let mode1_clicked = {Dom_html.mouseEvent Js.t->unit{ fun _ -> switch_mode Mode1 }} in
    let mode2_clicked = {Dom_html.mouseEvent Js.t->unit{ fun _ -> switch_mode Mode2 }} in
    let mode3_clicked = {Dom_html.mouseEvent Js.t->unit{ fun _ -> switch_mode Mode3 }} in
    div ~a:[a_class ["main-left-bar"]]
        [ div ~a:[a_class ["main-chat-area"]] [pcdata "chat"]
        ; div ~a:[a_class ["main-aux-buttons-area"]]
              [ input ~a:[a_class ["aux-button-radio"]; a_id "aux-button-mode1"; a_name "aux-buttons"
                         ; Unsafe.string_attrib "checked" "" ]
                      ~input_type:`Radio ()
              ; Unsafe.node "label" ~a:[ Unsafe.string_attrib "for" "aux-button-mode1"
                                       ; a_class ["aux-button-label"]
                                       ; a_onclick mode1_clicked
                                       ] [pcdata "Mode 1"]
              ; input ~a:[a_class ["aux-button-radio"]; a_id "aux-button-mode2"; a_name "aux-buttons"]
                      ~input_type:`Radio ()
              ; Unsafe.node "label" ~a:[ Unsafe.string_attrib "for" "aux-button-mode2"
                                       ; a_class ["aux-button-label"]
                                       ; a_onclick mode2_clicked] [pcdata "Mode 2"]
              ; input ~a:[ a_class ["aux-button-radio"]; a_id "aux-button-mode3"
                         ; a_name "aux-buttons" ]
                      ~input_type:`Radio ()
              ; Unsafe.node "label" ~a:[ Unsafe.string_attrib "for" "aux-button-mode3"
                                       ; a_class ["aux-button-label"]
                                       ; a_onclick mode3_clicked] [pcdata "Mode 3"]
              ; Unsafe.node "a" []
              ]
        ]
  in
  ignore {unit Lwt.t{
    let open Lwt_js_events in
    lwt () = clicks (To_dom.of_button %initdb_btn) (fun e _ -> %initdb_rpc ()) in

    Lwt.return ()
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
  let right_area =
    div ~a:[a_class ["main-right"]] [pcdata "right"]
  in

  ignore {unit{
              let params = Js.Unsafe.obj [||] in
              JQ.jq_selectable params (Ojquery.jQelt @@ Ojquery.js_jQ ".main-events-list")
         }};
  ignore {unit{ Lwt.ignore_result
    (lwt events = get_last_events () in
     Manip.appendChildren %events_list_div @@ List.map make_node_for_event events;
     Lwt.return ()
    )
  }};

  Lwt.return [ heading; left_area; center_view; right_area ]


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
                ; ["css"; "work.css"]
                ; ["css"; "jquery-ui.css"] ]
           Html5.F.(body ~a:[] xs)
    ))
