{client{
open Firebug
open Helpers
open Helpers_client
open Printf
open Js

type hash_t = ((js_string t) * (js_string t)) list

let read_hash () : hash_t =
  let split_amp s : hash_t =
    let arr = s##split (Js.string "&") |> Js.str_array |> Js.to_array in
    let f s =
      let pos = s##indexOf (Js.string "=") in
      if pos<0 then (s, Js.string "")
      else (s##slice (0, pos), s##slice_end (pos+1))
    in
    Array.fold_right (fun item acc -> f item :: acc) arr []
  in
  let hash = Dom_html.window##location##hash in
  let hash_pos = hash##indexOf (Js.string "#") in
  if hash_pos < 0 then []
  else split_amp (hash##slice_end (hash_pos+1) )

let set_helper' (k,v) xs =
  let postfix = List.fold_left xs
                  ~f:(fun acc (k,v) -> acc##concat_4 (Js.string "&", k, Js.string "=", v) )
                  ~init:(Js.string "")
  in
  let hash = Dom_html.window##location##hash in
  let prefix =
    match hash##indexOf (Js.string "#") with
    | -1 -> hash
    |  n -> hash##slice (0, n)
  in
  let ans = prefix##concat_4 (k, Js.string "=", v, postfix) in
  Dom_html.window##location##hash <- prefix##concat_2 (Js.string "#", ans);
  console##log (Dom_html.window##location##hash)

let remove_value k =
  let xs = read_hash () |> List.remove_assoc k in
  match xs with
  | [] -> Dom_html.window##location##hash <- Js.string ""
  | (k,v)::tl -> set_helper' (k,v) tl

let set_value ~key v =
  console##log_3 (Js.string "set_value", Js.string key, Js.string v);
  let k = Js.string key in
  let xs = read_hash () |> List.remove_assoc k (* |> ((::) (k, Js.string v) ) *) in
  set_helper' (k, Js.string v) xs

let get_value_exn (k: string) : js_string t =
  let key = Js.string k in
  List.Assoc.find_exn ~key (read_hash())

let get_value key =
  try Some(get_value_exn key)
  with Not_found -> None

let set_mode m =
  let string_of_mode = function
    | Common.Mode1 -> "1"
    | Common.Mode2 -> "2"
    | Common.Mode3 -> "3"
    | Common.Mode4 -> "4"
  in
  let xs = read_hash ()
           |> List.filter ~f:(fun (k,_) -> Js.to_string k <> "mode") in
  set_helper' (Js.string "mode", Js.string @@ string_of_mode m) xs


let detect_mode () : Common.mode =
  let xs = read_hash () in
  let ans =
    try (match List.assoc (Js.string "mode") xs |> Js.to_string with
         | "1" -> Some Common.Mode1
         | "2" -> Some Common.Mode2
         | "3" -> Some Common.Mode3
         | "4" -> Some Common.Mode4
         |  _  -> None)
    with Not_found -> None
  in
  Option.get ~default:Common.Mode1 ans

(*
let get_value key =
  let xs = read_hash () in
  List.Assoc.assoc (Js.string key) xs |> Js.to_string
 *)

   }}
