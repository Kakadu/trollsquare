{server{
open Printf
open Helpers_common

let string_of_char = String.make 1
let failwiths fmt = failwith (sprintf fmt)

module Result = Result
type ('a,'b) result = ('a,'b) Result.t


module String = struct
  include StringLabels

  let split ~on s = Str.(split (regexp @@ string_of_char on) s)
  let split_s ~on s = Str.(split (regexp on) s)
  let rsplit ~by s =
    try
      let i = rindex s by in
      sub s ~pos:(i+1) ~len:(String.length s - i - 1)
    with Not_found -> s

end


}}
