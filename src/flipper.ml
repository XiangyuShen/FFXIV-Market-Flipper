open Core
open Market

let args = (Sys.get_argv())
let call = Array.get args 1

let () =
if String.(=) "init" call then
  try let server = Array.get args 2 in
    init server
  with _ -> print_endline "Please init with a valid server name."
else if String.(=) "update" call then
  try let server = read_file "server.txt" in
    print_string "Loading...\n";
    update server;
    print_endline "Complete!"
  with _ -> print_endline "Please init first."
else if String.(=) "listings" call then
  try let flag = Array.get args 2 in
    if String.(=) "--margin" flag then
      listing 1
    else if String.(=) "--stacks" flag then
      listing 1
    else 
      print_endline "Please enter a valid flag.\n
      You can use --margin, --stacks, or none at all!"
  with _ -> 
    listing 0
else
  try let id = Int.of_string call in
    single @@ Int.to_string id
  with _ ->
    let id = id_of_name ~name:call in 
      single id