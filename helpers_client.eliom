{client{
open Printf

let firelog fmt = Firebug.console##log (Js.string @@ sprintf fmt)

module List = struct
  include ListLabels
  module Assoc = struct
    let find_exn ~key xs = assoc key xs
    let (_: key:'a -> ('a*'b) list -> 'b) = find_exn

    let find ~key xs = try  Some(find_exn ~key xs)
                       with Not_found -> None
  end
end
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
  external magic : 'a -> 'b = "%identity"
  external constant : string -> any = "caml_js_expr"
  external call_method : any -> string -> any array -> any = "caml_js_meth_call"
end
module Inject = struct
  let identity (i : 'a) : any = Ops.magic i
end
module Extract = struct
  let identity (i : any) : 'a = Ops.magic i
end

let alloc_args nb = Array.make nb (Ops.constant "undefined"), ref []
let set_arg args idx arg = (fst args).(idx) <- arg
let build_args args =
  Array.concat [
    fst args ;
    Array.of_list (List.rev !(snd args)) ;
  ]
let extract_t = Extract.identity


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

  let clear obj =
    let args = alloc_args 0 in
    let res = Ops.call_method obj "clear" (build_args args) in
    extract_t res

  module Sel = struct
    let show s = show @@ jQelt (Js.string s)
    let hide s = hide @@ jQelt (Js.string s)
  end

end

}}
