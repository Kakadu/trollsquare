{client{
open Printf
open Firebug
open Helpers

include Helpers_common

let firelog fmt = console##log (Js.string @@ sprintf fmt)
let printf fmt  = console##log (Js.string @@ sprintf fmt)
let print_endline s = console##log (Js.string s)

module ODates = ODate.Make(ODate.MakeImplem(Unix))


module String = struct
  include StringLabels
  let string_after s c =
    let pos: int = String.index s c in
    Firebug.console##log (Js.string @@ sprintf "s = %s, pos = %d" s pos);
    if pos = -1 then s else String.sub s (pos+1) (String.length s - pos - 1)

  let index_opt s c =
    try Some (index s c)
    with Not_found -> None
end





let with_element_by_id_exn id ~ok ~bad =
  Js.Opt.case (Dom_html.document##getElementById (Js.string id) )
    (fun () -> bad id)
    ok

let get_element_by_id_exn id =
  let o = Dom_html.document##getElementById (Js.string id) in
  Js.Opt.get o  (fun () -> failwith (sprintf "Can't get element with id='%s'" id) )

type any = Ojquery.t
module Ops = struct
  external to_bool : any -> bool = "caml_js_to_bool"
  external magic : 'a -> 'b = "%identity"
  external constant : string -> any = "caml_js_expr"
  external call_method : any -> string -> any array -> any = "caml_js_meth_call"
  external to_string : any -> string = "caml_js_to_string"
end
module Inject = struct
  let identity (i : 'a) : any = Ops.magic i
end
module Extract = struct
  let identity (i : any) : 'a = Ops.magic i
  let int (i : any) : int = Ops.magic i
  let unit (i : any) : unit = Ops.magic ()
  let float (i : any) : float = Ops.magic i
  let string (i : any) : string = Ops.to_string (Ops.magic i)
  let bool (i : any) : bool = Ops.to_bool (Ops.magic i)
end

let alloc_args nb = Array.make nb (Ops.constant "undefined"), ref []
let set_arg args idx arg = (fst args).(idx) <- arg
let build_args args =
  Array.concat [
    fst args ;
    Array.of_list (List.rev !(snd args)) ;
  ]
let extract_t = Extract.identity
(*
module Option = struct
    let iter ~f o = match o with Some x -> f x | None -> ()
end
 *)
module JQ = struct
  class type datepicker_options = object
    method dateFormat: Js.js_string Js.t Js.prop
  end

  let jq_datepicker properties obj =
    let args = alloc_args 1 in
    set_arg args 0 (Inject.identity properties);
    let res = Ops.call_method obj "datepicker" (build_args args) in
    extract_t res

  class type timepicker_options = object
    method step: Js.js_string Js.t Js.prop
    method timeFormat: Js.js_string Js.t Js.prop
  end
  let jq_timepicker properties obj =
    let args = alloc_args 1 in
    set_arg args 0 (Inject.identity properties);
    let res = Ops.call_method obj "timepicker" (build_args args) in
    extract_t res

  let jq_tooltip obj =
    let args = alloc_args 0 in
    let res = Ops.call_method obj "tooltip" (build_args args) in
    extract_t res

  class type selectable_options = object
    (*method step: Js.js_string Js.t Js.prop
    method timeFormat: Js.js_string Js.t Js.prop*)
  end

  let jq_selectable properties obj =
    let args = alloc_args 1 in
    set_arg args 0 (Inject.identity properties);
    let res = Ops.call_method obj "selectable" (build_args args) in
    extract_t res

  let jQelt e = Js.Unsafe.fun_call (Js.Unsafe.variable "jQuery")
    [| Js.Unsafe.inject e |]

  let append_element (el: Dom_html.element Js.t) parent =
    let args = alloc_args 1 in
    set_arg args 0 (Inject.identity el) ;
    let res = Ops.call_method parent "append" (build_args args) in
    extract_t res

  let hide obj =
    let args = alloc_args 0 in
    let res = Ops.call_method obj "hide" (build_args args) in
    extract_t res

  let show obj =
    let args = alloc_args 0 in
    let res = Ops.call_method obj "show" (build_args args) in
    extract_t res

  (*  Remove all child nodes of the set of matched elements from the DOM. *)
  let clear obj =
    let args = alloc_args 0 in
    let res = Ops.call_method obj "empty" (build_args args) in
    extract_t res

  let val_ obj =
    let args = alloc_args 0 in
    let res = Ops.call_method obj "val" (build_args args) in
    Extract.string res

  let set_val obj text =
    let args = alloc_args 1 in
    set_arg args 0 (Inject.identity @@ Js.string text) ;
    let res = Ops.call_method obj "val" (build_args args) in
    ()

  let attr obj (name: string) (newval: string) =
    let args = alloc_args 2 in
    set_arg args 0 (Inject.identity @@ Js.string name) ;
    set_arg args 1 (Inject.identity @@ Js.string newval) ;
    let res = Ops.call_method obj "attr" (build_args args) in
    extract_t res

  let tooltip obj =
    let args = alloc_args 0 in
    let res = Ops.call_method obj "tooltip" (build_args args) in
    ignore @@ extract_t res

  module Sel = struct
    let show s = show @@ jQelt (Js.string s)
    let hide s = hide @@ jQelt (Js.string s)
    let clear s = clear @@ jQelt (Js.string s)
  end

  class type jqui_dialog = object
    method show  : unit Js.meth
    method close : unit Js.meth
  end

  let dialog ?width ?height ~title ~buttons selector : jqui_dialog Js.t =
    let buttons =
      Js.Unsafe.obj @@ Array.of_list @@
        List.map buttons ~f:(fun (name,f) -> (name, Js.Unsafe.inject @@ Js.wrap_callback f) )
    in
    let properties = Js.Unsafe.obj [||] in
    properties##buttons <- Js.Unsafe.inject buttons;
    properties##modal   <- Js.Unsafe.inject @@ Js._true;
    properties##title   <- Js.Unsafe.inject @@ Js.string title;
    Option.iter width  ~f:(fun (w: int) -> properties##width  <- Js.Unsafe.inject w);
    Option.iter height ~f:(fun (h: int) -> properties##height <- Js.Unsafe.inject h);

    let clas = "." ^ selector in
    (* dialog initialization *)
    let args = alloc_args 1 in
    set_arg args 0 (Inject.identity properties);
    let res = Ops.call_method (jQelt @@ Js.string clas) "dialog" (build_args args) in
    ignore @@ extract_t res;

    (* wrapping result object *)
    let ans = Js.Unsafe.obj [||] in
    let show (): unit =
      let obj = jQelt @@ Js.string clas in
      let args = alloc_args 1 in
      set_arg args 0 (Inject.identity @@ Js.string "open");
      let res = Ops.call_method obj "dialog" (build_args args) in
      ignore @@ extract_t res
    in
    let close (): unit =
      let obj = jQelt @@ Js.string clas in
      let args = alloc_args 1 in
      set_arg args 0 (Inject.identity @@ Js.string "close");
      let res = Ops.call_method obj "dialog" (build_args args) in
      ignore @@ extract_t res
    in
    ans##show  <- Js.Unsafe.inject @@ Js.wrap_callback show;
    ans##close <- Js.Unsafe.inject @@ Js.wrap_callback close;
    ans

end

open Eliom_content.Html5.F
let dummy_img ?a ?(alt="") () =
  let a = match a with
  | Some a -> a
  | None   -> []
  in
  img ~a ~src:(Xml.uri_of_string "") ~alt ()


}}
