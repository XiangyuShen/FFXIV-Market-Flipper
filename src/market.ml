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
type item = (string * listing * listing * string * int * margin) [@@deriving yojson]


(* Deconstruct list of Yojson items*)

let deconstruct_json_string_list (l: Yojson.Basic.t list): string list =
  List.fold_left l ~init:[] ~f:(fun acc x -> (Yojson.Basic.Util.to_string x |> String.lowercase)::acc)

let deconstruct_json_int_list (l: Yojson.Basic.t list): int list =
  List.fold_left l ~init:[] ~f:(fun acc x -> (Yojson.Basic.Util.to_int x)::acc)

let deconstruct_json_item_list (l: Yojson.Safe.t list): item list =
  List.fold_left l ~init:[] ~f:(fun acc x -> 
    match item_of_yojson x with
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

let [@coverage off] read_data _: item list =
  let data = read_file "data.txt" in
  deconstruct_json_item_list (Yojson.Safe.Util.to_list @@ Yojson.Safe.from_string data)

(*Save data to file*)
let [@coverage off] write_data (l:item list): _ =
  write_file "data.txt" @@ List.to_string l ~f:(fun a -> 
  Yojson.Safe.to_string @@ item_to_yojson a)

(* Translate item name to id and vice versa *)
let name_of_id ~id:(id:string): string =
  let req = Client.get (Uri.of_string (xiv_URL^"item/"^id)) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body
    in
  let open Yojson.Basic.Util in
  Lwt_main.run req |> Yojson.Basic.from_string |> member "Name" |> to_string

let id_of_name ~name:(name:string): string = 
  let req = Client.get (Uri.of_string (xiv_URL^"search?string="^name)) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body
    in
  let open Yojson.Basic.Util in
  Lwt_main.run req |> Yojson.Basic.from_string |> member "Results" |> to_list |> List.hd_exn |> member "ID" |> to_int |> Int.to_string

(* Get prices on user's server*)
let [@coverage off] prices_on_server ~server:(server:string) ~item:(item:string): listing =
  let req = Client.get (Uri.of_string (univ_URL^server^"/"^item)) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body
  in
  let open Yojson.Basic.Util in
  let listings = Lwt_main.run req |> Yojson.Basic.from_string |> member "listings" |> to_list
  in 
  if (List.length listings = 0) then (0,0) else
  (List.hd_exn listings |> member "pricePerUnit" |> to_int, List.hd_exn listings |> member "quantity" |> to_int)

(* Find the data center that contains the server the user chose *)
let rec find_dc (dcs: (string * Yojson.Basic.t) list) ~(server:string): string =
  match dcs with
  | [] -> failwith "Invalid Server" [@coverage off]
  | hd::tl -> match hd with
    | dc, servers -> if List.mem (Yojson.Basic.Util.to_list servers |> deconstruct_json_string_list) server ~equal:String.equal then dc else find_dc tl ~server:server

let get_dc (server:string): string =
  let dc_req = Client.get (Uri.of_string (xiv_URL^"servers/dc")) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body
    in 
    Lwt_main.run dc_req |> Yojson.Basic.from_string |> Yojson.Basic.Util.to_assoc |> find_dc ~server:server

(* Get prices on the user's data center*)
let [@coverage off] prices_on_dc ~dc:(dc:string) ~item:(item:string): listing * string =
  let price_req = Client.get (Uri.of_string (univ_URL^dc^"/"^item)) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body in
  let open Yojson.Basic.Util in
  let top_listing = Lwt_main.run price_req |> Yojson.Basic.from_string |> member "listings" |> to_list |> List.hd_exn in
  let price =  top_listing |> member "pricePerUnit" |> to_int in
  let quant = top_listing |> member "quantity" |> to_int in
  let world = top_listing |> member "worldName" |> to_string in
  ((price, quant), world)

(* Overarching functions for user requests *)

(* Initialize user server and create storage file *)
let [@coverage off] init (server:string): _ =
  write_file "server.txt" server

(* Grab all prices and process *)
let [@coverage off] update (server:string): _ =
  failwith "unimplemented"
  (*let market_req = Client.get (Uri.of_string (univ_URL^"marketable")) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body in
  let open Yojson.Basic.Util in
  let marketable = Lwt_main.run market_req |> Yojson.Basic.from_string |> to_list |> deconstruct_json_int_list in
  let item_list = List.fold_left marketable ~init:[] ~f:(fun acc x -> (prices_on_server "hyperion" @@ Int.to_string x)::acc) in
  write_data item_list*)
  

(* Grab listings with user specified conditions*)
let [@coverage off] listing (flags:string list): _ = 
  failwith "unimplemented"