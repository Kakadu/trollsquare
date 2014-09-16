{client{
open Helpers
open Eliom_content.Html5
open Helpers_client
open Printf


type dialog_info =
  { buttons: (string * (unit -> unit)) list
  ; title:   string
  ; height:  int
  ; width :   int
  ; selector: string
  ; content : [ Html5_types.div ] D.elt list
  }

let ds : dialog_info list ref = ref []

let dialogs : (string * Helpers_client.JQ.jqui_dialog Js.t) list ref = ref []

let register ~buttons ~selector ~width ~height ~title ~content =
  Printf.printf "registering '%s'" selector;
  let d = { buttons; title; height; width; selector; content } in
  Ref.replace ds (fun xs -> d::xs)

let init () =
  print_endline @@ sprintf "Dialogs.init ()";
  let f  { buttons; title; height; width; selector; content } =
    let node = D.(div ~a:[a_class [selector]] content) in
    let parent = Ojquery.(jQelt @@ js_jQ "body") in
    let (_: Ojquery.t) = JQ.append_element (To_dom.of_div node) parent in
    let d = JQ.dialog ~width ~height ~title ~buttons selector in
    d##close ();
    print_endline @@ sprintf "Adding select '%s'" selector;
    Ref.replace dialogs ~f:(fun ds -> (selector,d) :: ds);
  in
  List.iter !ds ~f;
  Ref.replace ds (fun _ -> [])


let get selector =
  let d = List.Assoc.find_exn !dialogs ~cond:(fun (d,_) -> d = selector) in
  (snd d)

let show selector =
  try let d = List.Assoc.find_exn !dialogs ~cond:(fun (d,_) -> d = selector) in
      (snd d)##show ()
  with Not_found -> print_endline @@ Printf.sprintf "No such dialog: %s\n" selector

let close selector =
  try let d = List.Assoc.find_exn !dialogs ~cond:(fun (d,_) -> d = selector) in
      (snd d)##close ()
  with Not_found -> print_endline @@ Printf.sprintf "No such dialog: %s\n" selector

let has_dialog selector =
  try ignore @@ List.Assoc.find_exn !dialogs ~cond:(fun (d,_) -> d = selector);
      true
  with Not_found -> false



}}
