{shared{

module Int = struct
  let compare: int -> int -> int = Pervasives.compare
  let max a b = if compare a b > 0 then a else b
end

module Exn = struct
  let to_string = Printexc.to_string
  let backtrace = Printexc.get_backtrace
end

module Ref = struct
  let replace r ~f =  r:= (f !r)
end

module Option = struct
  let map ~f = function Some x -> Some (f x) | None -> None
  let value ~f ~default = function Some x -> f x | None -> default
  let get ~default = function Some x -> x | None -> default
  let get_exn = function Some x -> x | None -> failwith "Bad argument of get_exn"
  let is_some = function Some _ -> true | None  -> false
  let is_none = function Some _ -> false | None  -> true
  let iter ~f = function Some x -> f x | None -> ()
end

module List = struct
  include ListLabels

  let find_exn ~cond xs = find ~f:cond xs
  let (_: cond:('a -> bool) -> 'a list -> 'a) = find_exn

  let find ~cond xs =
    try Some (find_exn ~cond xs)
    with Not_found -> None

  let concat_strings ~sep ~f xs =
    let b = Buffer.create 100 in
    iter xs ~f:(fun item -> Buffer.add_string b (f item); Buffer.add_char b sep);
    Buffer.contents b

  let to_string ~f xs =
    Printf.sprintf "(%s)" (concat_strings ~sep:' ' ~f xs)

  let initi n ~f =
    let rec loop acc n =
      if n<0 then acc else loop ((f n)::acc) (n-1)
    in
    loop [] n

  let filter_map ~f xs =
    fold_left ~init:[] ~f:(fun acc x -> match f x with Some y -> y::acc | None -> acc) xs
  let filter_mapi ~f xs =
    let i = ref 0 in
    filter_map xs ~f:(fun x -> let ans = f !i x in incr i; ans)


  let hd_exn = function
    | x::_ -> x
    | [] -> failwith "Bad argument of hd_exn"
  let tl_exn = function
    | x::_ -> x
    | [] -> failwith "Bad argument of tl_exn"
  let rec last_exn = function
    | [x] -> x
    | x::xs -> last_exn xs
    | [] -> failwith "Bad argument of last_exn"

  module Assoc = struct
    let assoc_exn k ~set = ListLabels.assoc k set
    let assoc k ~set = try Some (assoc_exn ~set k) with Not_found -> None
    let mem_exn ~set el = ListLabels.mem ~set el
    let mem ~set el = try Some(mem_exn ~set el) with Not_found -> None

    let find ~cond xs =
      let f = fun acc x ->
        match acc with
        | None when cond x -> Some x
        | None  -> None
        | Some _ -> acc
      in
      fold_left ~init:None ~f xs

    let find_exn ~cond xs = match find ~cond xs with Some x -> x | None -> raise Not_found
    let remove ~key xs =
      filter xs ~f:(fun (k,_) -> k <> key)
  end

end

   }}
