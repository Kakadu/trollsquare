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
  let make yy mm dd hh mi ss =
    Calendar.(make yy mm dd hh mi ss |> to_unixfloat |> int_of_float)
  in
  [ { e_timestamp = make 2014 07 17 19 02 00
    ; e_title="Ukraine relocates SA-17 Grizzly to Ukrainian-Russian border"
    ; e_desc=""
    ; e_url ="http://anti-maidan.com/index.php?p=news&id=3957"
    }
  ; { e_timestamp = make 2014 07 17 20 00 00
    ; e_title="Boeing 777 of Malaisia airlaines crashes near Ukrainian-Russian border"
    ; e_desc=""
    ; e_url= "" }

  ; { e_timestamp = make 2014 07 17 22 00 00
    ; e_title="Mass-Media: The real target of Ukranian missle was V.Putin's airplane"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&id=3965" }
  ; { e_timestamp = make 2014 07 17 22 19 00
    ; e_title="Flight recorders are found on planecrash area"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&id=3966" }
  ; { e_timestamp = make 2014 07 17 22 51 00
    ; e_title="Flightradar24: before its dissappearing Boeing was located near city of Kremenchug"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&id=3968" }
  ; { e_timestamp = make 2014 07 17 23 20 00
    ; e_title="CNN: Are separatists able to destrot Boeing?"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&id=3967" }
  ; { e_timestamp = make 2014 07 18 00 13 00
    ; e_title="V.Putin accuses Ukraine in Boeing catastophe"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&id=3971" }
  ; { e_timestamp = make 2014 07 18 00 25 00
    ; e_title="Donetsk People's Republic is concluding local armistice near plane's crash area"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&id=3973" }
  ; { e_timestamp = make 2014 07 18 01 14 00
    ; e_title="Spanish dispatcher have seen Ukrainian Air Forces near crashed Boeing"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&id=3976" }
  ; { e_timestamp = make 2014 07 18 08 15 00
    ; e_title="OSCE is calling to seal off zone of aircrash"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&id=3994" }
  ; { e_timestamp = make 2014 07 18 14 14 00
    ; e_title="DNR: Kiev's attempts to claim us in Boeing catastrophe are awkward"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&id=3988" }
  ; { e_timestamp = make 2014 07 18 20 37 00
    ; e_title="OSCE wathers have got limited access to the area of aircrash"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&id=4009" }
  ; { e_timestamp = make 2014 07 18 21 48 00
    ; e_title="John Kirby: The Pentagon doesn't know who have destroyed the Boeing MH17"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&id=4011" }
  ; { e_timestamp = make 2014 07 18 21 55 00
    ; e_title="Journalist: Boeing was flying at 480 km to north comparately to normal route"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&id=4013" }
  ; { e_timestamp = make 2014 07 19 04 43 00
    ; e_title="Karakas: The catastrophe of Boeing is result of actions of USA"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&id=4025" }
  ; { e_timestamp = make 2014 07 19 06 38 00
    ; e_title="S.Lavrov: Russia will not decypher 'black boxes' on its own territory"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&id=4028" }
  ; { e_timestamp = make 2014 07 19 07 10 00
    ; e_title="China and Argentina call to objective investigation of aircrash"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&id=4030" }
  ; { e_timestamp = make 2014 07 19 13 27 00
    ; e_title="Ministry of Defense of Russian asks Kiev 10 questions about Boeing"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&id=4038" }
  ; { e_timestamp = make 2014 07 19 15 19 00
    ; e_title="DNR vice prime minister: Kiev prevents aircrash investigation"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4042" }
  ; { e_timestamp = make 2014 07 20 02 58 00
    ; e_title="DNR gurantees safety for experts on Boeing crash area"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4068" }
  ; { e_timestamp = make 2014 07 20 08 03 00
    ; e_title="OSCE delegation works on aircrash area"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4076" }
  ; { e_timestamp = make 2014 07 20 23 45 00
    ; e_title="LNR;s boss of anti-espionage: The purpose of Ukrainian air forses was to crash Boeing on Russian territory"
    ; e_desc=""
    ; e_url= "" }
  ; { e_timestamp = make 2014 07 21 09 05 00
    ; e_title="IKAO experts arrive to Kiev"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4128" }
  ; { e_timestamp = make 2014 07 21 11 36 00
    ; e_title="Experts from Netherlands arrive to city of Torez"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4133" }
  ; { e_timestamp = make 2014 07 21 15 36 00
    ; e_title="DNR: Arriving of Malaisian experts can be postpones becuase skirmishes at Donetsk"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4147" }
  ; { e_timestamp = make 2014 07 21 17 24 00
    ; e_title="Congressman: The crash of MH17 is a result of US's support of Maidan"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4157" }
  ; { e_timestamp = make 2014 07 21 18 23 00
    ; e_title="B.Obama comments situation in the Ukraine"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4160" }
  ; { e_timestamp = make 2014 07 21 18 36 00
    ; e_title="Press conference of Malaisian prime minister"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4168" }
  ; { e_timestamp = make 2014 07 21 20 30 00
    ; e_title="Pentagon doesn't beleive MD of Russia"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4171" }
  ; { e_timestamp = make 2014 07 21 22 59 00
    ; e_title="UN's conference about aircrash0"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4174" }
  ; { e_timestamp = make 2014 07 21 23 09 00
    ; e_title="What US is hiding when they don't show photos from satellite"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4179" }
  ; { e_timestamp = make 2014 07 22 01 28 00
    ; e_title="Plane recorded is transferred to Malaisian delegation"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4186" }
  ; { e_timestamp = make 2014 07 22 09 16 00
    ; e_title="R.Perry: American satellite shows that Boeing was destroyed by Ukrainian military forces"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4192" }
  ; { e_timestamp = make 2014 07 22 19 00 00
    ; e_title="The Finantioal Times presents eveidences that MH17 was destroyed by missle "
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4216" }
  ; { e_timestamp = make 2014 07 22 19 20 00
    ; e_title="US Natianl departmes uses data from social networks as main source about MH117"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4217" }
  ; { e_timestamp = make 2014 07 23 00  41 00
    ; e_title="US has not eveidences that Russia is connected to Boeing's crash"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4240" }
  ; { e_timestamp = make 2014 07 23 04 31 00
    ; e_title="Poll about Boeing on ukrainian news web-site"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4244" }
  ; { e_timestamp = make 2014 07 23 11 31 00
    ; e_title="Facts presented by US natianal depatment raise questions from journalists"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4251" }
  ; { e_timestamp = make 2014 07 23 20 40 00
    ; e_title="OSCE missions: militia have granted full access to all objects of airchrash"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4281" }
  ; { e_timestamp = make 2014 07 23 21 00 00
    ; e_title="Malaisia accuses US at using MH117 crash in their geopolitical interests"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4291" }
  ; { e_timestamp = make 2014 07 24 05 26 00
    ; e_title="A.Sharij have found SA-11 claimed by ukrainin air forces as Russian."
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4311" }
  ; { e_timestamp = make 2014 07 24 05 51 00
    ; e_title="Ukraine will not give out recorders between air traffic controllers and MH117"
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4317" }
  ; { e_timestamp = make 2014 07 24 08 50 00
    ; e_title="IKAO: Flyght recorder of speech is in good shape "
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4323" }
  ; { e_timestamp = make 2014 07 24 20 00 00
    ; e_title="Rusian Ministry of Defense: all connects between crash of MH117 and militia are based on information from social networks "
    ; e_desc=""
    ; e_url= "http://anti-maidan.com/index.php?p=news&cat=2&id=4329" }

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
