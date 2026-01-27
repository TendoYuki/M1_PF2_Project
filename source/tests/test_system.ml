open OUnit2
open Libnewtonoid

module G = Game

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                    *)
(* -------------------------------------------------------------------------- *)

let set_ball s b = { s with ball = b }
let set_bricks s br = { s with bricks = br }
let set_lives s l = { s with lives = l }

(* -------------------------------------------------------------------------- *)
(* Tests système                                                               *)
(* -------------------------------------------------------------------------- *)

(* 1. La balle doit avancer correctement *)
let test_step_safe _ =
  let s = Game.init_state () in
  let b = { s.ball with x = 100.; y = 100.; vx = 5.; vy = 0. } in
  let s = set_ball s b in
  let s2 = Game.step s in
  assert_bool "La balle doit avancer vers la droite" (s2.ball.x > s.ball.x)

(* 2. Perte de vie quand la balle tombe *)
let test_life_loss _ =
  let s = Game.init_state () in
  let b = { s.ball with y = -10. } in
  let s = set_ball s b in
  let s2 = Game.step s in
  assert_equal (s.lives - 1) s2.lives

(* 3. Rebond sur le mur gauche *)
let test_wall_bounce_left _ =
  let s = Game.init_state () in
  let b = { s.ball with x = 1.; vx = -5. } in
  let s = set_ball s b in
  let s2 = Game.step s in
  assert_bool "La balle doit rebondir vers la droite" (s2.ball.vx > 0.)

(* 4. Destruction d’une brique *)
let test_brick_destroy _ =
  let s = Game.init_state () in
  let brick = List.hd s.bricks in
  let b = { s.ball with x = brick.x; y = brick.y; vx = 0.; vy = -5. } in
  let s = set_ball s b in
  let s2 = Game.step s in
  assert_equal
    (List.length s.bricks - 1)
    (List.length s2.bricks)

(* 5. Passage au niveau suivant *)
let test_next_level _ =
  let s = Game.init_state () in
  let s = set_bricks s [] in
  let s2 = Game.step s in
  assert_equal 2 s2.level

(* 6. La balle reste collée si stuck = true *)
let test_stuck _ =
  let s = Game.init_state () in
  let s = { s with stuck = true } in
  let s2 = Game.step s in
  assert_bool "La balle doit rester collée" s2.stuck

(* 7. Rebond sur le plafond *)
let test_ceiling_bounce _ =
  let s = Game.init_state () in
  let b = { s.ball with y = Constants.box_supy -. 1.; vy = 5. } in
  let s = set_ball s b in
  let s2 = Game.step s in
  assert_bool "La balle doit rebondir vers le bas" (s2.ball.vy < 0.)

(* -------------------------------------------------------------------------- *)
(* Suite de tests                                                             *)
(* -------------------------------------------------------------------------- *)

let suite =
  "Newtonoid System Tests" >::: [
    "step_safe" >:: test_step_safe;
    "life_loss" >:: test_life_loss;
    "wall_bounce_left" >:: test_wall_bounce_left;
    "brick_destroy" >:: test_brick_destroy;
    "next_level" >:: test_next_level;
    "stuck" >:: test_stuck;
    "ceiling_bounce" >:: test_ceiling_bounce;
  ]

let () = run_test_tt_main suite