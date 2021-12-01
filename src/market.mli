(* Raw margin, then percent margin*)
type margin = (int * float)

(*Price, quantity*)
type listing = (int * int)

(* Name, lowest listing on server, lowest listing on data center, 
lowest price server name, date last sold on server, margin*)
type item = (string * listing * listing * string * int * margin)

(* Note: We currently plan to order the list of items when the users 
calls for listings. Depending on how this goes, we may instead choose 
a sorted list or binary tree of some sort and sort during update*)

(* Translate item name to id and vice versa *)
val name_of_id : id:string -> string

val id_of_name : name:string -> string

(* Get prices on user's server*)
val prices_on_server : server:string -> item:string -> listing

(* Deconstruct list of Yojson items to strings for specific API request result*)
val deconstruct_json_string_list: l:(Yojson.Basic.t list) -> string list

(* Find the data center that contains the server the user chose *)
val get_dc: server:string -> string

(* Get prices on the user's data center*)
val prices_on_dc: dc:string -> item:string -> listing * string

(* Overarching functions for user requests *)

(* Initialize user server and create storage file *)
val init: server:string -> _
(* Grab all prices and process *)
val update: server:string -> _
(* Grab listings with user specified conditions*)
val listing: flags:string list -> _


(*Calculate margins for each item*)
val calculate_margins: item:string -> home:int -> dc:int -> margin

(*Read data from file*)
val read_data: _ -> item list
(*Save data to file*)
val write_data: item list -> _