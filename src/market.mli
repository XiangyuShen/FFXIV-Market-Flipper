(* Raw margin, then percent margin*)
type margin = (int * float)

val margin_to_yojson : margin -> Yojson.Safe.t
val margin_of_yojson : Yojson.Safe.t -> margin Ppx_deriving_yojson_runtime.error_or

(*Price, quantity*)
type listing = (int * int)

val listing_to_yojson : listing -> Yojson.Safe.t
val listing_of_yojson : Yojson.Safe.t -> listing Ppx_deriving_yojson_runtime.error_or

(* Name, lowest listing on server, lowest listing on data center, 
lowest price server name, date last sold on server, margin*)
type item = (string * listing * listing * string * int * margin)

val item_to_yojson : item -> Yojson.Safe.t
val item_of_yojson : Yojson.Safe.t -> item Ppx_deriving_yojson_runtime.error_or

(*Calculate margins for each item*)
val calculate_margins: home:int -> dc:int -> margin

(*Read data from file*)
val read_data: string -> item list
(*Save data to file*)
val write_data: string -> item list -> unit

val read_file: string -> string

(* Translate item name to id and vice versa *)
val name_of_id : id:string -> string

val id_of_name : name:string -> string

(* Get prices on user's server*)
val prices_on_server : server:string -> item:string -> listing * int

(* Deconstruct list of Yojson items to strings/ints for specific API request result*)
val deconstruct_json_string_list: (Yojson.Basic.t list) -> string list
val deconstruct_json_int_list: (Yojson.Basic.t list) -> int list

(* Find the data center that contains the server the user chose *)
val get_dc: string -> string

(* Get prices on the user's data center*)
val prices_on_dc: dc:string -> item:string -> listing * string

(* Overarching functions for user requests *)

(* Initialize user server and create storage file *)
val init: string -> unit
(* Grab all prices and process *)
val update: string -> unit
(* Grab listings with user specified conditions*)
val listing: int -> unit

(* Grabs the listing of a single item*)
val single: string -> unit