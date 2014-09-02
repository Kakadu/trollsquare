{server{
open Neorest
open Helpers
open Printf
open Result
open CalendarLib
open Types

module API = Neorest.Make(struct let server="localhost" let port=7474 end)

let make_nodes ?(verbose=false) events =
  let () = match API.remove_all () with
    | OK () -> ()
    | Error () -> fprintf stderr "Can't connect to database"; exit 1
  in

  let has_date ts =
    let cmd = "OPTIONAL MATCH (d:DAY) WHERE d.timestamp={ts} RETURN id(d)" in
    let params = [ "ts", `Int ts ] in
    API.wrap_cypher ~verbose cmd ~params ~f:(function
    | `List [ `List [`Int id ] ] -> OK (Some id)
    | `List [ `List [`Null   ] ] -> OK None
    | _ -> Error ""
    )
  in
  let get_prev ts =
    let cmd = "MATCH (e:DAY) WHERE e.timestamp < {ts}
               RETURN e.timestamp,id(e) ORDER BY e.timestamp DESC LIMIT 1"
    in
    let params = [ "ts", `Int ts ] in
    API.wrap_cypher ~verbose cmd ~params ~f:(function
    | `List[]  -> OK None
    | `List xs when List.length xs > 1  -> Error "Too many results"
    | `List[`List[ `Int _ts; `Int id] ] ->  OK (Some (_ts,id) )
    | _ -> Error "Wrong format"
    )
  in
  let get_next ts =
    let cmd = "MATCH (e:DAY) WHERE e.timestamp > {ts}
               RETURN e.timestamp,id(e) ORDER BY e.timestamp LIMIT 1
               "
    in
    let params = [ "ts", `Int ts ] in
    API.wrap_cypher ~verbose cmd ~params ~f:(function
    | `List[]  -> OK None
    | `List xs when List.length xs > 1  -> Error "Too many results"
    | `List[`List[ `Int _ts; `Int id] ] ->  OK (Some (_ts,id) )
    | _ -> Error "Wrong format"
    )
  in
  let create_day ts =
    let desc = ts |> float_of_int |> Calendar.from_unixfloat |> Calendar.to_date |> Printer.Date.to_string in
    let params = [ ("ts", `Int ts); ("desc", `String desc) ] in
    let cmd = "MERGE (ans:DAY{ timestamp: {ts}, desc: {desc} }) RETURN id(ans)" in
    API.wrap_cypher cmd ~params ~f:(function
    | `List[ `List[ `Int id ] ] -> OK id
    | _ -> Error ""
    )
  in
  let create_daylink ~from ~dest =
    let params = [ ("from", `Int from); ("dest", `Int dest) ] in
    let cmd = "START l=node({from}), r=node({dest}) MERGE l-[:NEXT_DAY]->r" in
    API.wrap_cypher ~verbose cmd ~params ~f:(fun _ -> OK ())
  in
  let connect_days = create_daylink in

  let create_event ~parentid _innerid e =
    let params = [ ("parentid", `Int parentid)
                 ; ("title", `String e.e_title)
                 (*; ("eventid", `Int innerid) *)
                 ; ("ts",   `Int e.e_timestamp)
                 ] in
    let cmd = "MERGE (id:UniqueId{name:'event'})
               ON CREATE SET id.count = 1
               ON MATCH SET id.count = id.count + 1
               WITH id.count AS uid_
               START day=node({parentid})
               CREATE day-[:HAS_EVENT]->(e:EVENT{title: {title}, timestamp: {ts}, uid: uid_ })
               RETURN id(e)
              " in
    API.wrap_cypher cmd ~params ~f:(fun _ ->
      OK ()
    )
  in
  let f = fun n ({e_desc; e_timestamp; e_title; _} as event) ->
    let day_ts = Calendar.(to_date @@ from_unixfloat @@ float_of_int e_timestamp) |> Date.to_unixfloat |> int_of_float in
    (*printf  "%s\n%d\n%!" e_title day_ts;*)
    let day_node_id : (int,_) result =
      has_date day_ts >>= function
      | None -> begin
          get_prev day_ts >>= fun prev_info ->
          get_next day_ts >>= fun next_info ->
          match prev_info,next_info with
            | None,None -> (* 1st node *)
              print_endline "Its a 1st node";
              create_day day_ts >>= fun _newid ->
              OK _newid
            | Some (prev_ts,prev_id), None ->
              (* We need to establish link between previous node and current one *)
              print_endline "There is prev. No next";
              create_day day_ts >>= fun new_id ->
              connect_days ~from:prev_id ~dest:new_id >>= fun () ->
              OK new_id
            | None,Some (next_ts,next_id) ->
              (* We need to establish link between previous node and current one *)
              create_day day_ts >>= fun new_id ->
              connect_days ~from:new_id ~dest:next_id >>= fun () ->
              OK new_id
            | Some (prev_ts,prev_id), Some (next_ts,next_id) ->
              (* We need to establish link between previous node and current one *)
              create_day day_ts >>= fun new_id ->
              connect_days ~from:new_id  ~dest:next_id >>= fun () ->
              connect_days ~from:prev_id ~dest:new_id  >>= fun () ->
              OK new_id
        end
      | Some id -> (* Date node already created. Do nothing *)
        OK id
    in

    let (_ : (_,_) result) = day_node_id >>= fun day_node_id ->
      (*printf "day_node_id=%d\n%!" day_node_id;*)
      create_event ~parentid:day_node_id n event >>= fun _ ->
      OK ()
    in
    ()
  in
  List.iteri events ~f

let events =
  (* TODO maybe add variable with level of fakeness *)
  let open Calendar in
  let ts yy mm dd hh mi ss =
    Calendar.(make yy mm dd hh mi ss |> to_unixfloat |> int_of_float)
  in
  let make ?(desc="") ?(shortcut="") ~ts ~title ~url =
    { e_timestamp = ts
    ; e_title = title
    ; e_desc = desc
    ; e_url =  url
    ; e_shortcut = shortcut
    }
  in
  [ make ~ts:(ts 2014 07 17 19 02 00)
         ~title:"Ukraine relocates SA-17 Grizzly to Ukrainian-Russian border"
         ~url:"http://anti-maidan.com/index.php?p=news&id=3957"
         ~desc:""
         ~shortcut:""

  ; make ~ts:(ts 2014 07 17 20 00 00)
         ~title:"Boeing 777 of Malaisia airlaines crashes near Ukrainian-Russian border"
         ~url:""

  ; make ~ts:(ts 2014 07 17 22 00 00)
         ~title:"Mass-Media: The real target of Ukranian missle was V.Putin's airplane"
         ~url: "http://anti-maidan.com/index.php?p=news&id=3965"
  ; make ~ts:(ts 2014 07 17 22 19 00)
         ~title:"Flight recorders are found on planecrash area"
         ~url: "http://anti-maidan.com/index.php?p=news&id=3966"
  ; make ~ts:(ts 2014 07 17 22 51 00)
         ~title:"Flightradar24: before its dissappearing Boeing was located near city of Kremenchug"
         ~url: "http://anti-maidan.com/index.php?p=news&id=3968"
  ; make ~ts:(ts 2014 07 17 23 20 00)
         ~title:"CNN: Are separatists able to destrot Boeing?"
         ~url: "http://anti-maidan.com/index.php?p=news&id=3967"
  ; make ~ts:(ts 2014 07 18 00 13 00)
         ~title:"V.Putin accuses Ukraine in Boeing catastophe"
         ~url: "http://anti-maidan.com/index.php?p=news&id=3971"
  ; make ~ts:(ts 2014 07 18 00 25 00)
         ~title:"Donetsk People's Republic is concluding local armistice near plane's crash area"
         ~url: "http://anti-maidan.com/index.php?p=news&id=3973"
  ; make ~ts:(ts 2014 07 18 01 14 00)
         ~title:"Spanish dispatcher have seen Ukrainian Air Forces near crashed Boeing"
         ~url: "http://anti-maidan.com/index.php?p=news&id=3976"
  ; make ~ts:(ts 2014 07 18 08 15 00)
         ~title:"OSCE is calling to seal off zone of aircrash"
         ~url: "http://anti-maidan.com/index.php?p=news&id=3994"
  ; make ~ts:(ts 2014 07 18 14 14 00)
         ~title:"DNR: Kiev's attempts to claim us in Boeing catastrophe are awkward"
         ~url: "http://anti-maidan.com/index.php?p=news&id=3988"
  ; make ~ts:(ts 2014 07 18 20 37 00)
         ~title:"OSCE wathers have got limited access to the area of aircrash"
         ~url: "http://anti-maidan.com/index.php?p=news&id=4009"
  ; make ~ts:(ts 2014 07 18 21 48 00)
         ~title:"John Kirby: The Pentagon doesn't know who have destroyed the Boeing MH17"
         ~url: "http://anti-maidan.com/index.php?p=news&id=4011"
  ; make ~ts:(ts 2014 07 18 21 55 00)
         ~title:"Journalist: Boeing was flying at 480 km to north comparately to normal route"
         ~url: "http://anti-maidan.com/index.php?p=news&id=4013"
  ; make ~ts:(ts 2014 07 19 04 43 00)
         ~title:"Karakas: The catastrophe of Boeing is result of actions of USA"
         ~url: "http://anti-maidan.com/index.php?p=news&id=4025"
  ; make ~ts:(ts 2014 07 19 06 38 00)
         ~title:"S.Lavrov: Russia will not decypher 'black boxes' on its own territory"
         ~url: "http://anti-maidan.com/index.php?p=news&id=4028"
  ; make ~ts:(ts 2014 07 19 07 10 00)
         ~title:"China and Argentina call to objective investigation of aircrash"
         ~url: "http://anti-maidan.com/index.php?p=news&id=4030"
  ; make ~ts:(ts 2014 07 19 13 27 00)
         ~title:"Ministry of Defense of Russian asks Kiev 10 questions about Boeing"
         ~url: "http://anti-maidan.com/index.php?p=news&id=4038"
  ; make ~ts:(ts 2014 07 19 15 19 00)
         ~title:"DNR vice prime minister: Kiev prevents aircrash investigation"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4042"
  ; make ~ts:(ts 2014 07 20 02 58 00)
         ~title:"DNR gurantees safety for experts on Boeing crash area"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4068"
  ; make ~ts:(ts 2014 07 20 08 03 00)
         ~title:"OSCE delegation works on aircrash area"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4076"
  ; make ~ts:(ts 2014 07 20 23 45 00)
         ~title:"LNR;s boss of anti-espionage: The purpose of Ukrainian air forses was to crash Boeing on Russian territory"
         ~url: ""
  ; make ~ts:(ts 2014 07 21 09 05 00)
         ~title:"IKAO experts arrive to Kiev"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4128"
  ; make ~ts:(ts 2014 07 21 11 36 00)
         ~title:"Experts from Netherlands arrive to city of Torez"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4133"
  ; make ~ts:(ts 2014 07 21 15 36 00)
         ~title:"DNR: Arriving of Malaisian experts can be postpones becuase skirmishes at Donetsk"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4147"
  ; make ~ts:(ts 2014 07 21 17 24 00)
         ~title:"Breifing of Ministry of Defense of Russia"
         ~url: "http://anti-maidan.com/index.php?p=news&id=4149"
  ; make ~ts:(ts 2014 07 21 17 24 00)
         ~title:"Congressman: The crash of MH17 is a result of US's support of Maidan"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4157"
  ; make ~ts:(ts 2014 07 21 18 23 00)
         ~title:"B.Obama comments situation in the Ukraine"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4160"
  ; make ~ts:(ts 2014 07 21 18 36 00)
    ~title:"Press conference of Malaisian prime minister"
    ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4168"
  ; make ~ts:(ts 2014 07 21 20 30 00)
         ~title:"Pentagon doesn't beleive MD of Russia"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4171"
  ; make ~ts:(ts 2014 07 21 22 59 00)
         ~title:"UN's conference about aircrash0"
         ~url: "Http://Anti-Maidan.Com/index.php?p=news&cat=2&id=4174"
  ; make ~ts:(ts 2014 07 21 23 09 00)
         ~title:"What US is hiding when they don't show photos from satellite"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4179"
  ; make ~ts:(ts 2014 07 22 01 28 00)
         ~title:"Plane recorded is transferred to Malaisian delegation"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4186"
  ; make ~ts:(ts 2014 07 22 09 16 00)
         ~title:"R.Perry: American satellite shows that Boeing was destroyed by Ukrainian military forces"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4192"
  ; make ~ts:(ts 2014 07 22 19 00 00)
         ~title:"The Finantioal Times presents eveidences that MH17 was destroyed by missle "
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4216"
  ; make ~ts:(ts 2014 07 22 19 20 00)
         ~title:"US Natianal department uses data from social networks as main source about MH117"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4217"
  ; make ~ts:(ts 2014 07 22 23 12 00)
         ~title:"LJ, stepasyuk: The facts about which Russian Ministry of defense is lying"
         ~url: "http://stepasyuk.livejournal.com/11192.html"
         ~shortcut:"stepasyuk1"
  ; make ~ts:(ts 2014 07 23 00  41 00)
         ~title:"US has not eveidences that Russia is connected to Boeing's crash"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4240"
  ; make ~ts:(ts 2014 07 23 04 31 00)
         ~title:"Poll about Boeing on ukrainian news web-site"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4244"
  ; make ~ts:(ts 2014 07 23 11 31 00)
         ~title:"Facts presented by US natianal depatment raise questions from journalists"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4251"
  ; make ~ts:(ts 2014 07 23 20 40 00)
         ~title:"OSCE missions: militia have granted full access to all objects of airchrash"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4281"
  ; make ~ts:(ts 2014 07 23 21 00 00)
         ~title:"Malaisia accuses US at using MH117 crash in their geopolitical interests"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4291"
  ; make ~ts:(ts 2014 07 24 05 26 00)
         ~title:"A.Sharij have found SA-11 claimed by ukrainin air forces as Russian."
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4311"
  ; make ~ts:(ts 2014 07 24 05 51 00)
         ~title:"Ukraine will not give out recorders between air traffic controllers and MH117"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4317"
  ; make ~ts:(ts 2014 07 24 08 50 00)
         ~title:"IKAO: Flyght recorder of speech is in good shape "
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4323"
  ; make ~ts:(ts 2014 07 24 20 00 00)
         ~title:"Rusian Ministry of Defense: all connects between crash of MH117 and militia are based on information from social networks "
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4329"

  ]

let create_db () : unit Lwt.t =
  Lwt.return @@ make_nodes events
(*
let event_of_json = function
  | `Assoc xs ->
     let e_desc = List.Assoc.find_exn xs "desc" |> YoUtil.drop_string in
     let e_timestamp = List.Assoc.find_exn xs "timestamp" |> YoUtil.drop_int in
     let e_title = List.Assoc.find xs "title" |> Option.value ~default:"" ~f:YoUtil.drop_string in
     let e_url   = List.Assoc.find xs "url" |> Option.value ~default:"" ~f:YoUtil.drop_string in
     Types.({e_desc; e_timestamp; e_title; e_url})
  | _ -> assert false
 *)

let get_events (_:Types.timestamp) : string Lwt.t =
  print_endline "db.get_events";
  let now_ts = Calendar.(now() |> to_unixfloat |> int_of_float) in
  let cmd = "match (e:EVENT) WHERE e.timestamp <= {ts} RETURN e ORDER by e.timestamp DESC LIMIT 10" in
  let params = [ ("ts", `Int now_ts) ] in

  let s = API.make_n_commit cmd ~params  in
  let j = s |> to_json  in
  let open YoUtil in
  let j2 = j |> drop_assoc |> List.assoc "results" |> drop_list |> List.hd
           |> drop_assoc  |> List.assoc "data" |> drop_list
  in
  let j3 = List.map j2 ~f:(fun x -> x |> drop_assoc |> List.assoc "row" |> drop_list |> List.hd ) in
  let s = Yojson.to_string (`List j3) in
  print_endline s;
  Lwt.return s

(* uid is inner id *)
let event_by_uid uid : string Lwt.t =
  let cmd = "match (e:EVENT) WHERE e.uid = {uid} RETURN e" in
  let params = [ ("uid", `Int uid) ] in
  let s = API.make_n_commit cmd ~params in
  print_endline s;
  let j = s |> to_json  in
  let open YoUtil in
  let j2 = j |> drop_assoc |> List.assoc "results" |> drop_list |> List.hd
           |> drop_assoc  |> List.assoc "data" |> drop_list
  in
  let j3 = List.map j2 ~f:(fun x -> x |> drop_assoc |> List.assoc "row" |> drop_list |> List.hd ) in
  Lwt.return @@ Yojson.to_string @@ List.hd j3

}}
