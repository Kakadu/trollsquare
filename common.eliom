{client{

type mode =
  | Mode1 (* тупой режим *)
  | Mode2 (* красивый просмотр таймлайна *)
  | Mode3 (* режим TODO *)
  | Mode4 (* простомотри евента подробно *)


(* http://www.jqwidgets.com/jquery-ui-splitter/
   http://www.melonhtml5.com/demo/timeline/
*)
let (switch_mode_event, switch_mode) : (mode React.event * _) = React.E.create ()

   }}
