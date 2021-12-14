[1mdiff --git a/src/market.ml b/src/market.ml[m
[1mindex 1893b70..7cdba04 100644[m
[1m--- a/src/market.ml[m
[1m+++ b/src/market.ml[m
[36m@@ -161,7 +161,7 @@[m [mMake sure to run update to get the prices." in[m
  [m
 (* Grab all prices and process *)[m
 let [@coverage off] update: unit =[m
[31m-  let server = read_file "server.txt" in[m
[32m+[m[32m  try let server = read_file "server.txt" in[m
   let market_req = Client.get [m
     (Uri.of_string (univ_URL^"marketable")) >>= fun (_, body) ->[m
     body |> Cohttp_lwt.Body.to_string >|= fun body ->[m
[36m@@ -176,9 +176,11 @@[m [mlet [@coverage off] update: unit =[m
     let dc = get_dc server in [m
     let ((dcp, dcq), lowest) = prices_on_dc ~dc:dc ~item:id in[m
     let margin = calculate_margins ~home:servp ~dc:dcq in[m
[32m+[m[32m    print_string @@ Int.to_string x;[m
     (name, (servp, servq), (dcp, dcq), lowest, date, margin)::acc) [m
   in[m
   write_data item_list[m
[32m+[m[32mwith _ -> print_endline "Please init first"[m
   [m
 (* Helper function to write listings to command line*)[m
 let [@coverage off] listing_helper (filename: string) (margin: int) (server_name: string): unit =[m
