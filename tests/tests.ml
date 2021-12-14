open OUnit2
open Core
open Market

let test_name_of_id _ = 
  assert_equal (name_of_id ~id:"6") @@ "Lightning Shard";
  assert_equal (name_of_id ~id:"2006") @@ "Doctore's Crook";
  assert_equal (name_of_id ~id:"31457") @@ "Manor Shirt"

let test_id_of_name _ = 
  assert_equal (id_of_name ~name:"Lightning Shard") @@ "6";
  assert_equal (id_of_name ~name:"doctore's crook") @@ "2006";
  assert_equal (id_of_name ~name:"manor shirt") @@ "31457"

let invariant_id_and_name _ = 
  let invariant id = assert_equal id @@ id_of_name ~name:(name_of_id ~id:id) in
  invariant "11";
  invariant "870";
  invariant "1111"

let test_calculate_margins _ =
  assert_equal (calculate_margins ~home:4 ~dc:2) (2,1.);
  assert_equal (calculate_margins ~home:30000 ~dc:5000) (25000,5.);
  assert_equal (calculate_margins ~home:35000000 ~dc:10000000) (25000000,2.5)

let test_deconstruct_json_string_list _ =
  let temp = Yojson.Basic.from_string "[ \"a\", \"b\", \"c\", \"d\" ]" |> Yojson.Basic.Util.to_list in
  assert_equal (deconstruct_json_string_list temp) ["d";"c";"b";"a"]

let test_deconstruct_json_int_list _ =
  let temp = Yojson.Basic.from_string "[ 1, 2, 3, 4 ]" |> Yojson.Basic.Util.to_list in
  assert_equal (deconstruct_json_int_list temp) [4;3;2;1];;

let test_get_dc _ =
  assert_equal "Primal" @@ get_dc "hyperion";
  assert_equal "Crystal" @@ get_dc "balmung";
  assert_equal "Aether" @@ get_dc "cactuar";
  assert_equal "Elemental" @@ get_dc "tonberry";
  assert_equal "Light" @@ get_dc "odin"

  let invariant_listing _ = 
    let invariant listing1 = assert_equal listing1 @@ 
      (match listing_of_yojson (listing_to_yojson listing1) with 
      | Ok(x) -> x
      | Error _ -> failwith "Test Failed") in
    invariant (12,12);
    invariant (121111,1211111111);
    invariant (0,0)

  let invariant_margin _ = 
    let invariant margin1 = assert_equal margin1 @@ 
    (match margin_of_yojson (margin_to_yojson margin1) with 
    | Ok(x) -> x
    | Error _ -> failwith "Test Failed") in
    invariant (12,1.2);
    invariant (121111,1.211111111);
    invariant (0,0.)

  let invariant_item _ = 
    let invariant item1 = assert_equal item1 @@ 
    (match item_of_yojson (item_to_yojson item1) with 
    | Ok(x) -> x
    | Error _ -> failwith "Test Failed") in
    invariant ("name", (12,12), (123,1), "server", 12321312421, (12,1.2));
    invariant ("Ash Mask", (12,12344), (123,11), "Hyperion", 11, (12,1.2));
    invariant ("Null", (0,0), (0,0), "Null", 0, (0,0.))

let general_tests = 
  "Checkpoint Tests" >: test_list [
    "Test name_of_id" >:: test_name_of_id;
    "Test id_of_name" >:: test_id_of_name;
    "Tests Calculate Margins" >:: test_calculate_margins;
    "Test Deconstruct json String List" >:: test_deconstruct_json_string_list;
    "Test Deconstruct json Int List" >:: test_deconstruct_json_int_list;
    "Test Get DC" >:: test_get_dc
  ]

let invariant_tests = 
  "Invariant Tests" >: test_list [
    "Test Invariant id and name" >:: invariant_id_and_name;
    "Test Listing yojson" >:: invariant_listing;
    "Test Margin yojson" >:: invariant_margin;
    "Test Item yojson" >:: invariant_item;
  ]
let series = 
  "Market Flipper Tests" >::: [
    general_tests;
    invariant_tests;
  ]

let () =
run_test_tt_main series