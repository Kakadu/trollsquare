(* TODO-like mode *)

{shared{
let container_classname = "todo-view"
let class_names = ["."^container_classname]
   }}

{client{
open Helpers_client
open Eliom_content.Html5
open Eliom_content.Html5.D


let clear () =
  List.iter class_names ~f:(fun c -> ignore @@ Ojquery.(empty (jQelt @@ js_jQ c) ) );
  ()

let _onModeChanged =
  let toggleMode on =
    if on then
      begin
        Lochash.set_mode Common.Mode3;
        let parent = (Ojquery.jQelt @@ Ojquery.js_jQ @@ List.hd class_names) in
        let nodes =
          [ div [pcdata "TODO"]
          ]
        in
        List.iter nodes ~f:(fun e -> JQ.append_element (To_dom.of_element e) parent);
        List.iter JQ.Sel.show class_names
      end else begin
        (*firelog "hiding mode3";*)
        List.iter JQ.Sel.hide class_names;
        clear ()
      end
  in
  let f new_mode =
    toggleMode (new_mode = Common.Mode3);
    ()
  in
  React.E.map f Common.switch_mode_event

}}
