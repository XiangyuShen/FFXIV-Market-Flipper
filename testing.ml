open Core
open Lwt
open Cohttp_lwt_unix

(*
let body =
  Client.get (Uri.of_string "https://universalis.app/api/tax-rates?world=Hyperion") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body
*)

let server = Array.get (Sys.get_argv()) 1
let item_id = Array.get (Sys.get_argv()) 2
let item_name = String.chop_suffix_if_exists ~suffix:")" @@ String.chop_prefix_if_exists ~prefix:"(" @@ List.to_string ~f:(fun a -> a) @@ Array.to_list @@ Array.slice (Sys.get_argv()) 3 0
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

    let req = Client.get (Uri.of_string ("https://xivapi.com/search?string="^item_name^"&indexes=item")) >>= fun (_, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      body

let () =
  let prices_dc = Lwt_main.run prices_dc in
  let prices = Lwt_main.run prices_server in
  let name = Lwt_main.run name in
  let id_from_name = Lwt_main.run req in

  let json_prices_dc = Yojson.Basic.from_string prices_dc in
  let json_prices = Yojson.Basic.from_string prices in
  let json_name = Yojson.Basic.from_string name in
  let json_id = Yojson.Basic.from_string id_from_name in

  let open Yojson.Basic.Util in
  let listings = json_prices |> member "listings" |> to_list in
  let cheapest = if (List.length listings = 0) then "0" else List.hd_exn listings |> member "pricePerUnit" |> to_int |> Int.to_string in
  let history = json_prices |> member "recentHistory" |> to_list in
  let mostRecent = List.hd_exn history |> member "timestamp" |> to_int |> Int.to_float in
  let listings_dc = json_prices_dc |> member "listings" |> to_list in
  let cheapest_dc = if (List.length listings_dc = 0) then "DNE" else List.hd_exn listings_dc |> member "pricePerUnit" |> to_int |> Int.to_string in
  let name_string = json_name|> member "Name" |> to_string in
  let world_name = List.hd_exn listings_dc |> member "worldName" |> to_string in
  let timeSoldAgo = Core.Unix.strftime (Float.(-) (Unix.time()) mostRecent |> Unix.localtime) "%H:%M:%S" in
  let timeSold = Core.Unix.strftime (mostRecent |> Unix.localtime) "%m/%d/%Y, %H:%M:%S" in
  let item_id_from_name = json_id |> member "Results" |> to_list |> List.hd_exn |> member "Name" |> to_string in

  print_endline (item_id_from_name^"   " ^name_string^": \nCheapest on "^server^": " ^ cheapest ^ "\nCheapest on your Data Center: " ^ cheapest_dc^" on " ^ world_name^"\nLast sold on your server: " ^ timeSold ^ " (" ^ timeSoldAgo ^" ago)")