{client{

open Js

class type dbevent_js = object
  method timestamp: int  readonly_prop
  method title:     string readonly_prop
  method uid:       int readonly_prop
end

class type dbinterpret_js = object
  method itext:     string readonly_prop
  method iuid:      int    readonly_prop
end

class type dbquestion_js = object
  method quid:      int    readonly_prop
  method qtext:     string readonly_prop
  method interprets: dbinterpret_js t js_array t readonly_prop
end

class type conforms_conflicts = object
  method conforms  : dbevent_js t js_array t readonly_prop
  method conflicts : dbevent_js t js_array t readonly_prop
end

   }}
