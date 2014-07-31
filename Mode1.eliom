{client{
open Helpers_client


let _onModeChanged =
  let toggleMode1 on =
    List.iter (if on then JQ.Sel.show else JQ.Sel.hide) [".main-event-view"; ".main-events-list"; ".main-right"]
  in
  let f new_mode = toggleMode1 (new_mode = Common.Mode1) in
  React.E.map f Common.switch_mode_event



   }}
