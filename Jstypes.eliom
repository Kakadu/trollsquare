{client{

open Js

class type dbevent_js = object
  method timestamp: int readonly_prop
  method title:     string readonly_prop
end

   }}