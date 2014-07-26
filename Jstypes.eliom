{client{

open Js

class type dbevent_js = object
  method timestamp: int readonly_prop
  method title:     js_string t readonly_prop
end

   }}
