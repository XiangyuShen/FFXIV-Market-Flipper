open Core
open Lwt
open Cohttp_lwt_unix

[@@@ocaml.warning "-27"]

let xiv_URL = "https://xivapi.com/"

let univ_URL = "https://universalis.app/api/"

(* Raw margin, then percent margin*)
type margin = (int * float) [@@deriving yojson]

(*Price, quantity*)
type listing = (int * int) [@@deriving yojson]

(* Name, lowest listing on server, lowest listing on data center, 
lowest price server name, date last sold on server, margin*)
type item = (string * listing * listing 
            * string * int * margin) [@@deriving yojson]


(* Deconstruct list of Yojson items*)

let deconstruct_json_string_list (l: Yojson.Basic.t list): string list =
  List.fold_left l ~init:[] ~f:(fun acc x -> 
    (Yojson.Basic.Util.to_string x 
    |> String.lowercase)::acc)

let deconstruct_json_int_list (l: Yojson.Basic.t list): int list =
  List.fold_left l ~init:[] ~f:(fun acc x -> (Yojson.Basic.Util.to_int x)::acc)

let deconstruct_json_item_list (l: Yojson.Safe.t list): item list =
  List.fold_left l ~init:[] ~f:(fun acc x -> 
    let temp = Yojson.Safe.Util.to_string x |>
    Yojson.Safe.from_string in
    match item_of_yojson temp with
    | Ok(item) -> item::acc
    | Error msg -> failwith "Error Reading In Data") [@coverage off]

(*Calculate margins for each item*)
let calculate_margins ~home:(home:int) ~dc:(dc:int): margin =
  let raw = home - dc in
  (raw, (Float.(/) (Float.of_int raw) (Float.of_int dc)))

(*Read data from file*)
let read_file filename =
  let file = In_channel.create filename in
  let strings = In_channel.input_all file in
  In_channel.close file;
  strings

let write_file filename message =
  let oc = Out_channel.create filename in
  Printf.fprintf oc "%s" message;
  Out_channel.close oc

let [@coverage off] read_data filename: item list =
  let data = read_file filename in
  deconstruct_json_item_list (Yojson.Safe.Util.to_list 
                              @@ Yojson.Safe.from_string data)

(*Save data to file*)
let [@coverage off] write_data filename (l:item list): _ =
  let data = List.to_string l ~f:(fun a -> 
    (Yojson.Safe.to_string @@ item_to_yojson a)^",")
    |> String.chop_prefix_if_exists ~prefix:"(" 
    |> String.chop_suffix_if_exists ~suffix:")" in
  write_file filename @@ "["^data^"]"

(* Translate item name to id and vice versa *)
let name_of_id ~id:(id:string): string =
  let req = Client.get (Uri.of_string (xiv_URL^"item/"^id)) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body
    in
  let open Yojson.Basic.Util in
  Lwt_main.run req |> Yojson.Basic.from_string |> member "Name" |> to_string

let id_of_name ~name:(name:string): string = 
  let req = Client.get (Uri.of_string (xiv_URL^"search?indexes=item&string="^name)) 
  >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body
    in
  let open Yojson.Basic.Util in
  Lwt_main.run req |> Yojson.Basic.from_string |> member "Results" 
  |> to_list |> List.hd_exn |> member "ID" |> to_int |> Int.to_string

(* Get prices on user's server*)
let [@coverage off] prices_on_server ~server:(server:string) ~item:(item:string): listing * int =
  let req = Client.get (Uri.of_string (univ_URL^server^"/"^item)) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body
  in
  let open Yojson.Basic.Util in
  let result = Lwt_main.run req |> Yojson.Basic.from_string in
  let listings = result |> member "listings" |> to_list in
  let history = result |> member "recentHistory" |> to_list in
  if (List.length listings = 0) then ((0,0),0) else
  if (List.length history = 0) then ((0,0),0) else
  ((List.hd_exn listings |> member "pricePerUnit" 
    |> to_int, List.hd_exn listings |> member "quantity" |> to_int),
    List.hd_exn history |> member "timestamp" |> to_int)

(* Find the data center that contains the server the user chose *)
let rec find_dc (dcs: (string * Yojson.Basic.t) list) ~(server:string): string =
  match dcs with
  | [] -> failwith "Invalid Server" [@coverage off]
  | hd::tl -> match hd with
    | dc, servers -> 
      if List.mem (Yojson.Basic.Util.to_list servers 
        |> deconstruct_json_string_list) server ~equal:String.equal 
      then dc 
      else find_dc tl ~server:server

let get_dc (server:string): string =
  let dc_req = Client.get (Uri.of_string (xiv_URL^"servers/dc")) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body
    in 
    Lwt_main.run dc_req |> Yojson.Basic.from_string 
    |> Yojson.Basic.Util.to_assoc |> find_dc ~server:(String.lowercase server)

(* Get prices on the user's data center*)
let [@coverage off] prices_on_dc ~dc:(dc:string) ~item:(item:string): listing * string =
  let price_req = Client.get (Uri.of_string (univ_URL^dc^"/"^item)) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body in
  let open Yojson.Basic.Util in
  let listings = Lwt_main.run price_req |> Yojson.Basic.from_string 
     |> member "listings" |> to_list in
   if (List.length listings = 0) then ((0,0),"None") else
   let top_listing = listings |> List.hd_exn in
  let price =  top_listing |> member "pricePerUnit" |> to_int in
  let quant = top_listing |> member "quantity" |> to_int in
  let world = top_listing |> member "worldName" |> to_string in
  ((price, quant), world)

(* Overarching functions for user requests *)

(* Initialize user server and create storage file *)
let [@coverage off] init (server:string): unit =
  try let dc = get_dc server in
  write_file "server.txt" @@ String.lowercase server;
  let output = "Home server set to: "^server^" on "^dc^"!\n
  Make sure to run update to get the prices." in
  print_endline output
  with _ -> print_endline "Please init with a valid server name."

  let [@coverage off] single (id:string): unit =
  try let server = read_file "server.txt" in
    let name = name_of_id ~id:id in
    let ((servp, servq), date) = prices_on_server ~server:server ~item:id in
    let timeSoldAgo = Core.Unix.strftime (Float.(-) (Unix.time()) 
      (Int.to_float date) |> Unix.localtime) "%Hh:%Mm:%Ss" in
    let timeSold = Core.Unix.strftime 
      (Int.to_float date |> Unix.localtime) "%m/%d/%Y, %H:%M:%S" in
    let dc = get_dc server in 
    let ((dcp, dcq), lowest) = prices_on_dc ~dc:dc ~item:id in
    print_endline (name^
    ": \nCheapest on "^server^": "^(Int.to_string servp)^
    "\nCheapest on your Data Center: "^(Int.to_string dcp)^" on "^lowest^
    "\nLast sold on your server: "^timeSold^" ("^timeSoldAgo^" ago)")
  with _ -> print_endline "Please run init first."
 
(* Grab all prices and process *)
let [@coverage off] update (server: string): unit =
  let server = read_file "server.txt" in
  let market_req = Client.get 
    (Uri.of_string (univ_URL^"marketable")) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body in
  let open Yojson.Basic.Util in
  let marketable = Lwt_main.run market_req |> Yojson.Basic.from_string 
    |> to_list |> deconstruct_json_int_list in
  let item_list = List.fold_left marketable ~init:[] ~f:(fun acc x -> 
    let id = Int.to_string x in
    let name = name_of_id ~id:id in
    let ((servp, servq), date) = prices_on_server ~server:server ~item:id in
    let dc = get_dc server in 
    let ((dcp, dcq), lowest) = prices_on_dc ~dc:dc ~item:id in
    let margin = calculate_margins ~home:servp ~dc:dcp in
    print_endline @@ Int.to_string x;
    (name, (servp, servq), (dcp, dcq), lowest, date, margin)::acc) 
  in
  let sort_raw = List.sort item_list ~compare:(fun 
  (_, _, _, _, _, (raw, _))
  (_, _, _, _, _, (raw2, _))-> Int.compare raw raw2) in
  let sort_percent = List.sort item_list ~compare:(fun 
  (_, _, _, _, _, (_, perc))
  (_, _, _, _, _, (_, perc2))-> Float.compare perc perc2) in
  let (rawrest, raw5) = List.split_n sort_raw (List.length sort_raw - 5) in
  let (percrest, perc5) = List.split_n sort_percent (List.length sort_percent - 5) in
  write_data "stacks.txt" raw5;
  write_data "margins.txt" perc5;
  write_data "stacksrest.txt" raw5;
  write_data "marginsrest.txt" perc5
    
  
(* Helper function to write listings to command line*)
let [@coverage off] listing_helper (filename: string) (margin: int) (server_name: string): unit =
  let temp,_ = List.split_n (read_data filename) 5 in
  let dc = get_dc server_name in
  let header = 
  if margin = 1 then
    "____________________________________________________________________________
|         	 Percent Price Gain on "^dc^"\t\t\t\t\t|\n"
else
  "______________________________________________________________________________
|          	 Raw Price Difference on "^dc^"\t\t\t\t\t|\n"
in
  let printable = List.fold temp ~init:"" ~f:(fun acc x -> 
    let (name,(servp, servq),(dcp, dcq), lowest, date,(raw, percent)) = x in
    if margin = 0 then
        acc ^ "| " ^ server_name ^ ": " ^ name ^ " " ^ (Int.to_string servp) ^
        "\t\t | " ^ lowest ^ ": " ^ name ^ " "^ (Int.to_string dcp) ^ " \t|\n"
    else if margin = 1 then 
      acc ^ "| " ^ server_name ^ ": " ^ name ^ " " ^ (Int.to_string servp) ^
        "\t\t  | " ^ lowest ^ ": " ^ name ^ " "^ (Int.to_string dcp) ^ " \t|\n"
    else acc ^ "| " ^ server_name ^ ": " ^ name ^ " " 
        ^ (Int.to_string servp) ^ " x" ^ (Int.to_string servq) 
        ^ "\t\t  | " ^ lowest ^ ": " ^ name ^ " " 
        ^ (Int.to_string dcp) ^ " x" ^ (Int.to_string dcq) ^ " \t|\n")
  in
  let total = header ^ printable ^ 
  "|___________________________________________________________________________|"
in
  print_endline total
(* Grab listings with user specified conditions, flags contains an int
  1 being margin, 2 being stacks, and 0 being raw*)
let [@coverage off] listing (flags: int): unit = 
try let server = read_file "server.txt" in
  if flags = 0
    then listing_helper "stacks.txt" flags server
  else if flags = 1
    then listing_helper "margins.txt" flags server
  else listing_helper "stacks.txt" flags server 
with exn -> raise exn