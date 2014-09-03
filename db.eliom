{server{
open Neorest
open Helpers
open Printf
open Result
open CalendarLib
open Types

module API = Neorest.Make(struct let server="localhost" let port=7474 end)
module StringMap = Map.Make(String)

module Shortcuts = struct
  let shortcut_map : Types.id StringMap.t ref = ref StringMap.empty
  let add ~key v = Ref.replace shortcut_map ~f:(StringMap.add key v)
  let get_exn k = StringMap.find k !shortcut_map

end

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

  let create_event ~parentid _innerid e : (Types.id, string) Result.t =
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
    API.wrap_cypher cmd ~params ~f:(function
      | `List [`Int uid] -> OK uid
      | _  -> Error "wrong cypher format"
    )
  in
  let f = fun n ({e_desc; e_timestamp; e_title; e_shortcut; _} as event) ->
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
      create_event ~parentid:day_node_id n event >>= fun uid ->
      if e_shortcut <> "" then  Shortcuts.add e_shortcut uid;
      OK ()
    in
    ()
  in
  List.iteri events ~f

type raw_interp =
  { ri_text: string
  ; ri_shortcut: string
  ; ri_conflicts: string list
  ; ri_conforms:  string list
  }

type raw_question = string * raw_interp list
let create_question ~parent ~title is =
  let parent_shortcut = parent in
  let params =
    [ "parent", `String parent
    ]
  in
  let cmd = "MERGE (id:UniqueId{name:'event'})
             ON CREATE SET id.count = 1
             ON MATCH SET id.count = id.count + 1
             WITH id.count AS uid_
             START (e:EVENT{shortcut: {parent}})
             CREATE e-[:HAS_QUESTION]->(q:QUESTION{text: {qtext}})
             "
  in
  let f {ri_text; ri_shortcut; _} =
    sprintf "CREATE q-[:HAS_INTERPRET]->(:INTERPRET{ text: \"%s\"; shortcut: \"%s\" }) "
            ri_text ri_shortcut
  in
  let tail = String.concat ~sep:" " @@ List.map ~f is in
  let cmd = String.concat ~sep:" " [cmd; tail; " RETURN uid_"] in
  API.wrap_cypher cmd ~params ~f:(function
      | `List [`Int uid] -> OK uid
      | _  -> Error "wrong cypher format"
    )

let events =
  (* TODO maybe add variable with level of fakeness *)
  let open Calendar in
  let ts yy mm dd hh mi ss =
    Calendar.(make yy mm dd hh mi ss |> to_unixfloat |> int_of_float)
  in
  let make ?(desc="") ?(shortcut="") ?(url="") ~ts title =
    { e_timestamp = ts
    ; e_title = title
    ; e_desc = desc
    ; e_url =  url
    ; e_shortcut = shortcut
    }
  in
  [ make ~ts:(ts 2014 07 17 19 02 00)
         ~url:"http://anti-maidan.com/index.php?p=news&id=3957"
         ~shortcut:"mh17crash"
         "Ukraine relocates SA-17 Grizzly to Ukrainian-Russian border"

  ; make ~ts:(ts 2014 07 17 20 00 00)
         "Boeing 777 of Malaisia airlaines crashes near Ukrainian-Russian border"

  ; make ~ts:(ts 2014 07 17 22 00 00)
         ~url: "http://anti-maidan.com/index.php?p=news&id=3965"
         "Mass-Media: The real target of Ukranian missle was V.Putin's airplane"
  ; make ~ts:(ts 2014 07 17 22 19 00)
         ~url: "http://anti-maidan.com/index.php?p=news&id=3966"
         "Flight recorders are found on planecrash area"
  ; make ~ts:(ts 2014 07 17 22 51 00)
         ~url: "http://anti-maidan.com/index.php?p=news&id=3968"
         "Flightradar24: before its dissappearing Boeing was located near city of Kremenchug"
  ; make ~ts:(ts 2014 07 17 23 20 00)
         ~url: "http://anti-maidan.com/index.php?p=news&id=3967"
         "CNN: Are separatists able to destrot Boeing?"
  ; make ~ts:(ts 2014 07 18 00 13 00)
         ~url: "http://anti-maidan.com/index.php?p=news&id=3971"
         "V.Putin accuses Ukraine in Boeing catastophe"
  ; make ~ts:(ts 2014 07 18 00 25 00)
         ~url: "http://anti-maidan.com/index.php?p=news&id=3973"
         "Donetsk People's Republic is concluding local armistice near plane's crash area"
  ; make ~ts:(ts 2014 07 18 01 14 00)
         ~url: "http://anti-maidan.com/index.php?p=news&id=3976"
         "Spanish dispatcher have seen Ukrainian Air Forces near crashed Boeing"
  ; make ~ts:(ts 2014 07 18 08 15 00)
         ~url: "http://anti-maidan.com/index.php?p=news&id=3994"
         "OSCE is calling to seal off zone of aircrash"
  ; make ~ts:(ts 2014 07 18 14 14 00)
         ~url: "http://anti-maidan.com/index.php?p=news&id=3988"
         "DNR: Kiev's attempts to claim us in Boeing catastrophe are awkward"
  ; make ~ts:(ts 2014 07 18 20 37 00)
         ~url: "http://anti-maidan.com/index.php?p=news&id=4009"
         "OSCE wathers have got limited access to the area of aircrash"
  ; make ~ts:(ts 2014 07 18 21 48 00)
         ~url: "http://anti-maidan.com/index.php?p=news&id=4011"
         "John Kirby: The Pentagon doesn't know who have destroyed the Boeing MH17"
  ; make ~ts:(ts 2014 07 18 21 55 00)
         ~url: "http://anti-maidan.com/index.php?p=news&id=4013"
         "Journalist: Boeing was flying at 480 km to north comparately to normal route"
  ; make ~ts:(ts 2014 07 19 04 43 00)
         ~url: "http://anti-maidan.com/index.php?p=news&id=4025"
         "Karakas: The catastrophe of Boeing is result of actions of USA"
  ; make ~ts:(ts 2014 07 19 06 38 00)
         ~url: "http://anti-maidan.com/index.php?p=news&id=4028"
         "S.Lavrov: Russia will not decypher 'black boxes' on its own territory"
  ; make ~ts:(ts 2014 07 19 07 10 00)
         ~url: "http://anti-maidan.com/index.php?p=news&id=4030"
         "China and Argentina call to objective investigation of aircrash"
  ; make ~ts:(ts 2014 07 19 13 27 00)
         ~url: "http://anti-maidan.com/index.php?p=news&id=4038"
         "Ministry of Defense of Russian asks Kiev 10 questions about Boeing"
  ; make ~ts:(ts 2014 07 19 15 19 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4042"
         "DNR vice prime minister: Kiev prevents aircrash investigation"
  ; make ~ts:(ts 2014 07 20 02 58 00)
         "DNR gurantees safety for experts on Boeing crash area"
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4068"
  ; make ~ts:(ts 2014 07 20 08 03 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4076"
         "OSCE delegation works on aircrash area"
  ; make ~ts:(ts 2014 07 20 23 45 00)
         "LNR;s boss of anti-espionage: The purpose of Ukrainian air forses was to crash Boeing on Russian territory"
  ; make ~ts:(ts 2014 07 21 09 05 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4128"
         "IKAO experts arrive to Kiev"
  ; make ~ts:(ts 2014 07 21 11 36 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4133"
         "Experts from Netherlands arrive to city of Torez"
  ; make ~ts:(ts 2014 07 21 15 36 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4147"
         "DNR: Arriving of Malaisian experts can be postpones becuase skirmishes at Donetsk"
  ; make ~ts:(ts 2014 07 21 17 24 00)
         ~url: "http://anti-maidan.com/index.php?p=news&id=4149"
         "Breifing of Ministry of Defense of Russia"
  ; make ~ts:(ts 2014 07 21 17 24 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4157"
         "Congressman: The crash of MH17 is a result of US's support of Maidan"
  ; make ~ts:(ts 2014 07 21 18 23 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4160"
         "B.Obama comments situation in the Ukraine"
  ; make ~ts:(ts 2014 07 21 18 36 00)
    "Press conference of Malaisian prime minister"
    ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4168"
  ; make ~ts:(ts 2014 07 21 20 30 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4171"
         "Pentagon doesn't beleive MD of Russia"
  ; make ~ts:(ts 2014 07 21 22 59 00)
         ~url: "Http://Anti-Maidan.Com/index.php?p=news&cat=2&id=4174"
         "UN's conference about aircrash0"
  ; make ~ts:(ts 2014 07 21 23 09 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4179"
         "What US is hiding when they don't show photos from satellite"
  ; make ~ts:(ts 2014 07 22 01 28 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4186"
         "Plane recorded is transferred to Malaisian delegation"
  ; make ~ts:(ts 2014 07 22 09 16 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4192"
         "R.Perry: American satellite shows that Boeing was destroyed by Ukrainian military forces"
  ; make ~ts:(ts 2014 07 22 19 00 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4216"
         "The Finantioal Times presents eveidences that MH17 was destroyed by missle "
  ; make ~ts:(ts 2014 07 22 19 20 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4217"
         "US Natianal department uses data from social networks as main source about MH117"
  ; make ~ts:(ts 2014 07 22 23 12 00)
         ~url: "http://stepasyuk.livejournal.com/11192.html"
         ~shortcut:"stepasyuk1"
         "LJ, stepasyuk: The facts about which Russian Ministry of defense is lying"
  ; make ~ts:(ts 2014 07 23 00  41 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4240"
         "US has not eveidences that Russia is connected to Boeing's crash"
  ; make ~ts:(ts 2014 07 23 04 31 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4244"
         "Poll about Boeing on ukrainian news web-site"
  ; make ~ts:(ts 2014 07 23 11 31 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4251"
         "Facts presented by US natianal depatment raise questions from journalists"
  ; make ~ts:(ts 2014 07 23 20 40 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4281"
         "OSCE missions: militia have granted full access to all objects of airchrash"
  ; make ~ts:(ts 2014 07 23 21 00 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4291"
         "Malaisia accuses US at using MH117 crash in their geopolitical interests"
  ; make ~ts:(ts 2014 07 24 05 26 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4311"
         "A.Sharij have found SA-11 claimed by ukrainin air forces as Russian."
  ; make ~ts:(ts 2014 07 24 05 51 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4317"
         "Ukraine will not give out recorders between air traffic controllers and MH117"
  ; make ~ts:(ts 2014 07 24 08 50 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4323"
         "IKAO: Flyght recorder of speech is in good shape "
  ; make ~ts:(ts 2014 07 24 20 00 00)
         ~url: "http://anti-maidan.com/index.php?p=news&cat=2&id=4329"
         "Rusian Ministry of Defense: all connects between crash of MH117 and militia are based on information from social networks "

  ; make ~ts:(ts 2014 08 26 07 56 00)
         ~url:"http://lenta.ru/news/2014/08/26/sbu/"
         ~shortcut:"paratroopers_ukr"
         "Ukraine spotted a number of Russian paratroopers on their territory"

  ; make ~ts:(ts 2014 08 26 18 25 00)
         ~url:"http://lenta.ru/news/2014/08/26/minobr/"
         "Russian Ministry of defense have explained paratroopers on ukranian territory"

  ; make ~ts:(ts 2014 08 25 21 52 00)
         ~url:"http://www.newsru.com/russia/25aug2014/pskov.html"
         ~shortcut:"pskov_paratroopers_buried"
         "A few paratroopers a buried near city of Pskov. Journalists said that they were fighting in the Ukraine."

  ; make ~ts:(ts 2014 08 25 21 52 00)
         ~url:"http://www.snob.ru/selected/entry/80262?preview=print"
         ~shortcut:"pskov_paratroopers_kremlin"
         "The Kremlin have explained the funeral of paratroopers in Pskov"

  ; make ~ts:(ts 2014 08 26 18 18 00)
         ~url:"http://inforesist.org/na-kladbishhe-gde-poxoronili-pskovskix-desantov-izbili-zhurnalistov/"
         "The journalists were beaten near the cemetery where paratroopers were buried."

  ; make ~ts:(ts 2014 08 27 11 11 11)
         ~url:"http://www.politonline.ru/?area=rssArticleItem&id=22481088&mode=print"
         ~shortcut:"pskov_paratroopers_callback"
         "Quazi-killed paratroopers are calling home."

  ; make ~ts:(ts 2014 08 27 15 15 15)
         ~url:""
         ~shortcut:"pskov_paratroopers_call_to_wife"
         "The wife of paratrooper says that he is at home and not dead."

  ; make ~ts:(ts 2014 08 31 00 45 00)
         ~url:"http://lenta.ru/news/2014/08/31/freedom/"
         ~shortcut:"paratroopers_ukr_back"
         "The paratroopers catched in the Ukraine have returned home."

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
