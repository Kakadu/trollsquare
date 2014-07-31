{client{

open Helpers
open Helpers_client
open Printf

let detect_mode () : Common.mode =
  let s = Dom_html.window##location##hash |> Js.to_string in
  let pos = String.index_opt s '#' in
  let ans =
  if pos = None then None else
    begin
      let arr = String.string_after s '#' in
      Firebug.console##log (Js.string arr);
      try
        let val' =
          (Js.string arr)##split (Js.string "&") |> Js.str_array |> Js.to_array |> Array.to_list
          |> List.find ~f:(fun js -> js##indexOf (Js.string "mode=") = 0)
        in
        Firebug.console##log_2 (Js.string "found: %s ", val');
        match String.string_after (Js.to_string val') '=' with
        | "1" -> Some Common.Mode1
        | "2" -> Some Common.Mode2
        | "3" -> Some Common.Mode3
        | "4" -> Some Common.Mode4
        |  _  -> None
      with Not_found -> firelog "no mode"; None
    end
  in
  Option.get ~default:Common.Mode1 ans



   }}
