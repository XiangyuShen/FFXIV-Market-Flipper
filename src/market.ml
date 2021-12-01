open Core
open Lwt
open Cohttp_lwt_unix

(* Raw margin, then percent margin*)
type margin = (int * float)

(*Price, quantity*)
type listing = (int * int)

(* Name, lowest listing on server, lowest listing on data center, 
lowest price server name, date last sold on server, margin*)
type item = (string * listing * listing * string * int * margin)

(* Translate item name to id and vice versa *)
let name_of_id (id:string): string =
  let req = Client.get (Uri.of_string ("https://xivapi.com/item/"^id)) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body
    in
  let open Yojson.Basic.Util in
  Lwt_main.run req |> Yojson.Basic.from_string |> member "Name" |> to_string

let id_of_name (name:string): string = 
  let req = Client.get (Uri.of_string ("https://xivapi.com/search?string="^name)) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body
    in
  let open Yojson.Basic.Util in
  Lwt_main.run req |> Yojson.Basic.from_string |> member "Results" |> to_string

(* Get prices on user's server*)
let prices_on_server (server:string) (item:string): string =
  let req = Client.get (Uri.of_string ("https://universalis.app/api/"^server^"/"^item)) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body
  in
  let open Yojson.Basic.Util in
  let listings = Lwt_main.run req |> Yojson.Basic.from_string |> member "listings" |> to_list
  in 
  if (List.length listings = 0) then "0" else
  List.hd_exn listings |> member "pricePerUnit" |> to_int |> Int.to_string

(* Deconstruct list of Yojson items to strings for specific API request result*)
let deconstruct_json_string_list (l:Yojson.Basic.t list): string list =

(* Find the data center that contains the server the user chose *)
let find_dc (dcs: (string * Yojson.Basic.t) list) (server:string): string =

let get_dc (server:string): string =

(* Get prices on the user's data center*)
let prices_on_dc (dc:string) (item:string): string =

(* Overarching functions for user requests *)

(* Initialize user server and create storage file *)
let init (server:string): _ =
(* Grab all prices and process *)
let update (server:string): _ =
(* Grab listings with user specified conditions*)
let listing (flags:string list): _ = 


(*Calculate margins for each item*)
let calculate_margins (item:string) (home:int) (dc:int): margin =

(*Read data from file*)
let read_data _: item list =
(*Save data to file*)
let write_data item list: _ =



let name =
  Client.get (Uri.of_string ("https://xivapi.com/item/"^item_id)) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  body

let prices_server =
  Client.get (Uri.of_string ("https://universalis.app/api/"^server^"/"^item_id )) >>= fun (_, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  body

let rec deconstruct_json_string_list (l: Yojson.Basic.t list): string list =
  match l with
  | [] -> []
  | hd::tl -> (Yojson.Basic.Util.to_string hd |> String.lowercase) :: deconstruct_json_string_list tl

let rec check_dc (dcs: (string * Yojson.Basic.t) list): string =
  match dcs with
  | [] -> failwith "Invalid Server"
  | hd::tl -> match hd with
    | dc, servers -> if List.mem (Yojson.Basic.Util.to_list servers |> deconstruct_json_string_list) server ~equal:String.equal then dc else check_dc tl

  let prices_dc =
    let dc_req = Client.get (Uri.of_string ("https://xivapi.com/servers/dc")) >>= fun (_, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      body
      in 
      let dc =
      Lwt_main.run dc_req |> Yojson.Basic.from_string |> Yojson.Basic.Util.to_assoc |> check_dc in
    (*NOW MATCH SERVER TO DC*)
    Client.get (Uri.of_string ("https://universalis.app/api/"^dc^"/"^item_id)) >>= fun (_, body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    body

    let () =
  let prices_dc = Lwt_main.run prices_dc in
  let prices = Lwt_main.run prices_server in
  let name = Lwt_main.run name in

  let json_prices_dc = Yojson.Basic.from_string prices_dc in
  let json_prices = Yojson.Basic.from_string prices in
  let json_name = Yojson.Basic.from_string name in

  let open Yojson.Basic.Util in
  let listings = json_prices |> member "listings" |> to_list in
  let cheapest = List.hd_exn listings |> member "pricePerUnit" |> to_int |> Int.to_string in
  let history = json_prices |> member "recentHistory" |> to_list in
  let mostRecent = List.hd_exn history |> member "timestamp" |> to_int |> Int.to_float in
  let listings_dc = json_prices_dc |> member "listings" |> to_list in
  let cheapest_dc = List.hd_exn listings_dc |> member "pricePerUnit" |> to_int |> Int.to_string in
  let name_string = json_name|> member "Name" |> to_string in
  let world_name = List.hd_exn listings_dc |> member "worldName" |> to_string in
  let timeSoldAgo = Core.Unix.strftime (Float.(-) (Unix.time()) mostRecent |> Unix.localtime) "%H:%M:%S" in
  let timeSold = Core.Unix.strftime (mostRecent |> Unix.localtime) "%m/%d/%Y, %H:%M:%S" in
  print_endline (name_string^": \nCheapest on "^server^": " ^ cheapest ^ "\nCheapest on your Data Center: " ^ cheapest_dc^" on " ^ world_name^"\nLast sold on your server: " ^ timeSold ^ " (" ^ timeSoldAgo ^" ago)")