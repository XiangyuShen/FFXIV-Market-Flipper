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
  print_string "Loading...\n";
  update;
  print_endline "Complete!"
else if String.(=) "listings" call then
  try let flag = Array.get args 2 in
    listing [String.(=) "--margin" flag]
  with _ -> 
    listing []
else
  try let id = Int.of_string call in
    single id
  with _ ->
    let id = id_of_name in 
      single @@ Int.of_string id