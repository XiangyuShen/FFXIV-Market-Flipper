open Core
open Market

let () =
let (raw, _) = calculate_margins ~home:2 ~dc:4 in
failwith ("unimplemented"^(Int.to_string raw))