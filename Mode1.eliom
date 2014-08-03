{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
}}

let get_last_event_rpc
    : (Types.timestamp, string) Eliom_pervasives.server_function
  = server_function Json.t<Types.timestamp> Db.get_events

{client{
open Helpers_client
open Jstypes
open Firebug
open Eliom_content.Html5

let get_last_events () : dbevent_js Js.t list Lwt.t =
  let ts = (jsnew Js.date_now ())##valueOf () in
  lwt s = %get_last_event_rpc (int_of_float ts) in
  let ans = Array.to_list @@ Js.to_array @@ Json.unsafe_input @@ Js.string s in
  Lwt.return ans

let make_node_for_event e =
  let ts = ODates.(From.seconds e##timestamp |> To.string Printer.default) in
  li ~a:[a_class ["event-list-item"]]
    [ div ~a:[a_class ["event-list-item-title"]] [pcdata @@ e##title]
    ; div ~a:[a_class ["event-list-item-time"]] [pcdata ts]
  ]

let clear () =
  Ojquery.(remove (jQelt @@ js_jQ ".event-list-item") ())


let _onModeChanged =
  let toggleMode1 on =
    let names =  [".main-event-view"; ".main-events-list"; ".main-right"] in
    if on then
      begin
        Lochash.set_mode Common.Mode1;
        List.iter JQ.Sel.show names
      end else
        List.iter JQ.Sel.hide names
  in
  let f new_mode =
    Lochash.set_mode Common.Mode1;
    toggleMode1 (new_mode = Common.Mode1);
    (lwt events = get_last_events () in
     let nodes = List.map ~f:make_node_for_event events in
     let parent = (Ojquery.jQelt @@ Ojquery.js_jQ ".main-events-list") in
     List.iter nodes ~f:(fun e -> JQ.append_element (To_dom.of_element e) parent);
     Lwt.return ()
    ) |>  Lwt.ignore_result
  in
  React.E.map f Common.switch_mode_event



}}
