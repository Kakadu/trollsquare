(* Take inspirations from there: http://medleyweb.com/inspiration/20-examples-timeline-web-design
 *)
{server{
open Eliom_lib
open Eliom_content
open Html5.D
open Types

let get_last_events_rpc
    : (Types.timestamp, string) Eliom_pervasives.server_function
  = server_function Json.t<Types.timestamp> Db.get_events

}}

{client{
open Helpers_client
open Jstypes
open Eliom_content.Html5
open Eliom_content.Html5.D
open Types

let show () = List.iter JQ.Sel.show [".main-timeline"]
let hide () = List.iter JQ.Sel.hide [".main-timeline"]

(* It is mode 2 *)

let get_last_events () : dbevent_js Js.t list Lwt.t =
  let ts = (jsnew Js.date_now ())##valueOf () in
  lwt s = %get_last_events_rpc (int_of_float ts) in
  Lwt.return (Array.to_list @@ Js.to_array @@ Json.unsafe_input @@ Js.string s)

let clear () =
  let () = Ojquery.(remove (jQelt @@ js_jQ ".timeline-element") ()) |> ignore in
  ()

let refresh () =
  let make_node isleft ev =
    let ts = ODates.(From.seconds ev##timestamp |> To.string Printer.default) in
    div ~a:[a_class [ "timeline-element"
                    ; (if isleft then "timeline-element-left" else "timeline-element-right")
           ] ]
        [ div ~a:[a_class ["timeline-element-box"] ]
              [ div ~a:[a_class ["timeline-element-title"]]
                    [ span ~a:[a_class ["timeline-title-label"]] [ pcdata "LABEL HERE" ]
                    ; span ~a:[a_class ["timeline-title-date"]] [ pcdata ts ]
                    ]
              ; div ~a:[a_class ["timeline-element-content"]] [pcdata @@ ev##title]
              ; div ~a:[a_class ["timeline-element-more"]] [pcdata "MOAR"]
              ]
        ]
  in
  let timeline = Ojquery.jQelt @@ Ojquery.js_jQ ".main-timeline" in
  (lwt xs = get_last_events () in
   List.iteri (fun n e ->
               let el = make_node (n mod 2=0) e in
               JQ.append_element (To_dom.of_div el) timeline
              ) xs;
   Lwt.return ()
  ) |> Lwt.ignore_result

let _onModeChanged =
  let toggleMode on =
    if on
    then (show (); Lochash.set_mode Common.Mode2)
    else hide ()
  in
  let f new_mode =
    clear ();
    toggleMode (new_mode = Common.Mode2);
    refresh ()
  in
  React.E.map f Common.switch_mode_event


}}
