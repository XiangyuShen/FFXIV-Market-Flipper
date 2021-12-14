open Core
open Market



let () =
let args = (Sys.get_argv()) in
if List.length (Array.to_list args) = 1 then
  print_endline "Please enter an option."
else 
  let call = Array.get args 1 in
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
        listing 2
      else 
        print_endline "Please enter a valid flag.
  You can use --margin, --stacks, or none at all!"
    with _ -> 
      listing 0
  else
    try let id = Int.of_string call in
      single @@ Int.to_string id
    with _ ->
      let (_, remaining_args) = List.split_n (Array.to_list args) 1 in
      let name = List.to_string remaining_args ~f:(fun a -> a) 
      |> String.chop_prefix_if_exists ~prefix:"(" 
      |> String.chop_suffix_if_exists ~suffix:")" in
      let id = id_of_name ~name:name in 
        single id