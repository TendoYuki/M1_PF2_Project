open OUnit2
open Libnewtonoid

(* --------------------------------- Helpers --------------------------------- *)

let show_vec v =
  Printf.sprintf "(x=%.3f, y=%.3f)" (Vector.x v) (Vector.y v)

let print_header title =
  Printf.printf "\n=== %s ===\n%!" title

(*--------------------------------- Tests Vector --------------------------------- *)

let test_vector_make _ =
  print_header "Vector.make";
  let v = Vector.make 3. 4. in
  Printf.printf "Créé: %s\n%!" (show_vec v);
  assert_equal 3. (Vector.x v);
  assert_equal 4. (Vector.y v)

let test_vector_add _ =
  print_header "Vector.add";
  let v1 = Vector.make 1. 2. in
  let v2 = Vector.make 3. 4. in
  let v = Vector.add v1 v2 in
  Printf.printf "v1=%s  v2=%s  v1+v2=%s\n%!"
    (show_vec v1) (show_vec v2) (show_vec v);
  assert_equal 4. (Vector.x v);
  assert_equal 6. (Vector.y v)

let test_vector_scale _ =
  print_header "Vector.scale";
  let v = Vector.make 2. 3. in
  let v2 = Vector.scale 2. v in
  Printf.printf "v=%s  2*v=%s\n%!" (show_vec v) (show_vec v2);
  assert_equal 4. (Vector.x v2);
  assert_equal 6. (Vector.y v2)

(*--------------------------------- Tests Physics.integrate --------------------------------- *)

let test_integrate _ =
  print_header "Physics.integrate";
  let pos = Vector.make 0. 0. in
  let vel = Vector.make 10. 0. in
  let acc = Vector.make 0. 0. in
  let new_pos, new_vel = Physics.integrate pos vel acc 1. in
  Printf.printf "pos=%s vel=%s → new_pos=%s new_vel=%s\n%!"
    (show_vec pos) (show_vec vel)
    (show_vec new_pos) (show_vec new_vel);
  assert_equal 10. (Vector.x new_pos);
  assert_equal 0. (Vector.y new_pos);
  assert_equal 10. (Vector.x new_vel)
  
(*--------------------------------- Tests Collision.circle_aabb --------------------------------- *)

let test_collision_hit _ =
  print_header "Collision.circle_aabb (hit)";
  let center = Vector.make 5. 5. in
  let rect = (8., 5., 10., 10.) in
  Printf.printf "center=%s radius=5 rect=(8,5,10,10)\n%!" (show_vec center);
  assert_bool "Collision attendue"
    (Collision.circle_aabb center 5. rect)

let test_collision_miss _ =
  print_header "Collision.circle_aabb (miss)";
  let center = Vector.make 0. 0. in
  let rect = (10., 10., 5., 5.) in
  Printf.printf "center=%s radius=2 rect=(10,10,5,5)\n%!" (show_vec center);
  assert_bool "Pas de collision attendue"
    (not (Collision.circle_aabb center 2. rect))

(*--------------------------------- Tests Constants --------------------------------- *)

let test_constants _ =
  print_header "Constants";
  Printf.printf "dt=%f gravity=%s\n%!"
    Constants.dt (show_vec Constants.gravity);
  assert_equal (1. /. 60.) Constants.dt;
  assert_equal (-20.) (Vector.y Constants.gravity)

(*--------------------------------- Suite de tests --------------------------------- *)

let suite =
  "Newtonoid Unit Tests" >::: [
    "vector_make" >:: test_vector_make;
    "vector_add" >:: test_vector_add;
    "vector_scale" >:: test_vector_scale;
    "integrate" >:: test_integrate;
    "collision_hit" >:: test_collision_hit;
    "collision_miss" >:: test_collision_miss;
    "constants" >:: test_constants;
  ]

let () = run_test_tt_main suite