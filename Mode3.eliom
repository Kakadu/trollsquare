(* TODO-like mode *)

{shared{
let class_names = ["todo-view"]
   }}

{client{
open Helpers_client
open Eliom_content.Html5
open Eliom_content.Html5.D


let clear () =
  List.iter [".todo-view"] ~f:(fun c -> ignore @@ Ojquery.(empty (jQelt @@ js_jQ c) ) );
  ()

let _onModeChanged =
  let toggleMode on =
    if on then
      begin
        Lochash.set_mode Common.Mode3;
        let parent = (Ojquery.jQelt @@ Ojquery.js_jQ ".todo-view") in
        let nodes =
          [ div [pcdata "TODO"]
          ]
        in
        List.iter nodes ~f:(fun e -> JQ.append_element (To_dom.of_element e) parent);
        List.iter JQ.Sel.show class_names
      end else begin
        firelog "hiding mode3";
        List.iter JQ.Sel.hide class_names;
        clear ()
      end
  in
  let f new_mode =
    toggleMode (new_mode = Common.Mode3);
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

}}
