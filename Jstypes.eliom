{client{

open Js

class type dbevent_js = object
  method timestamp: int  readonly_prop
  method title:     string readonly_prop
  (*method eventid:   int  readonly_prop *)
  method uid:       int readonly_prop
end

   }}
