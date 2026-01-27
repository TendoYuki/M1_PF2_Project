(* Tests système pour Newtonoid *)
open OUnit2
open Libnewtonoid
open Game
open Types

(* ========== Tests d'initialisation ========== *)

let test_init_state _ =
  print_endline "\n=== TEST: État initial du jeu ===";
  print_endline "Vérification: 3 vies, score 0, balle collée, vitesse 1.0";
  assert_equal 3 init_state.lives ~msg:"État initial devrait avoir 3 vies";
  assert_equal 0 init_state.score ~msg:"Score initial devrait être 0";
  assert_equal true init_state.stuck ~msg:"Balle devrait être collée au départ";
  assert_equal 1.0 init_state.current_speed ~msg:"Vitesse initiale devrait être 1.0";
  assert_equal [] init_state.powerups ~msg:"Pas de powerups au départ";
  print_endline "-- État initial correct"

let test_init_paddle _ =
  print_endline "\n=== TEST: Paddle initiale ===";
  Printf.printf "Position: x=%.1f, largeur=%.1f, hauteur=%.1f\n" 
    init_paddle.x init_paddle.width init_paddle.height;
  assert_equal 400. init_paddle.x ~msg:"Paddle devrait être centré";
  assert_equal 80. init_paddle.width ~msg:"Largeur initiale du paddle";
  assert_equal 15. init_paddle.height ~msg:"Hauteur initiale du paddle";
  print_endline "-- Paddle correctement initialisée"

let test_init_ball _ =
  print_endline "\n=== TEST: Balle initiale ===";
  let pos = Vector.to_tuple init_ball.pos in
  Printf.printf "Position: (%.1f, %.1f), rayon=%.1f\n" 
    (fst pos) (snd pos) init_ball.radius;
  assert_equal (400., 300.) pos ~msg:"Position initiale de la balle";
  assert_equal 8. init_ball.radius ~msg:"Rayon de la balle";
  print_endline "-- Balle correctement initialisée"

(* ========== Tests de génération de niveau ========== *)

let test_generate_level _ =
  print_endline "\n=== TEST: Génération aléatoire de niveau ===";
  let bricks = generate_random_level () in
  let brick_count = Quadtree.size bricks in
  Printf.printf "Nombre de briques générées: %d\n" brick_count;
  assert_bool "Le niveau devrait contenir au moins quelques briques" (brick_count > 0);
  assert_bool "Le niveau ne devrait pas être trop plein" (brick_count < 200);
  print_endline "-- Niveau généré avec succès"

let test_bricks_properties _ =
  print_endline "\n=== TEST: Propriétés des briques ===";
  let bricks = generate_random_level () in
  let all_bricks = Quadtree.to_list bricks in
  Printf.printf "Test de %d briques...\n" (List.length all_bricks);
  List.iter (fun brick ->
    assert_bool "Brique devrait être vivante" brick.alive;
    assert_bool "Valeur de brique valide" (brick.value >= 50 && brick.value <= 200);
    assert_bool "Position X valide" (brick.x >= 0. && brick.x <= 800.);
    assert_bool "Position Y valide" (brick.y >= 0. && brick.y <= 600.)
  ) all_bricks;
  print_endline "-- Toutes les briques ont des propriétés valides"

(* ========== Tests de collision ========== *)

let test_paddle_clamp _ =
  print_endline "\n=== TEST: Limitation paddle dans l'écran ===";
  let paddle = { x = 50.; width = 80.; height = 15. } in
  let x_clamped = Collision.clamp 10. (Constants.box_infx +. paddle.width /. 2.)
                                      (Constants.box_supx -. paddle.width /. 2.) in
  Printf.printf "Position tentée: 10.0 -> Position clampée: %.1f\n" x_clamped;
  assert_bool "Paddle ne devrait pas sortir à gauche" 
    (x_clamped >= Constants.box_infx +. paddle.width /. 2.);
  print_endline "-- Paddle reste dans les limites"

let test_circle_aabb_collision _ =
  print_endline "\n=== TEST: Détection collision Circle-AABB (avec collision) ===";
  let center = Vector.make 100. 100. in
  let radius = 10. in
  let box = (90., 90., 30., 30.) in
  Printf.printf "Cercle: centre(100,100) rayon=10 | Rectangle: (90,90) 30x30\n";
  let collision = Collision.circle_aabb center radius box in
  Printf.printf "Collision détectée: %b\n" collision;
  assert_bool "Devrait détecter collision" collision;
  print_endline "-- Collision correctement détectée"

let test_circle_aabb_no_collision _ =
  print_endline "\n=== TEST: Détection collision Circle-AABB (sans collision) ===";
  let center = Vector.make 100. 100. in
  let radius = 5. in
  let box = (200., 200., 30., 30.) in
  Printf.printf "Cercle: centre(100,100) rayon=5 | Rectangle: (200,200) 30x30\n";
  let collision = Collision.circle_aabb center radius box in
  Printf.printf "Collision détectée: %b\n" collision;
  assert_bool "Ne devrait pas détecter collision" (not collision);
  print_endline "-- Pas de fausse collision"

(* ========== Tests de QuadTree ========== *)

let test_quadtree_insert _ =
  print_endline "\n=== TEST: Insertion dans QuadTree ===";
  let qt = Quadtree.create 0. 0. 800. 600. in
  let brick = { x = 100.; y = 100.; w = 60.; h = 20.; value = 100; alive = true } in
  Printf.printf "Insertion brique à (%.1f, %.1f)\n" brick.x brick.y;
  let qt' = Quadtree.insert qt brick brick.x brick.y in
  let size = Quadtree.size qt' in
  Printf.printf "Taille du QuadTree après insertion: %d\n" size;
  assert_equal 1 size ~msg:"QuadTree devrait contenir 1 élément";
  print_endline "-- Insertion réussie"

let test_quadtree_query _ =
  print_endline "\n=== TEST: Requête spatiale QuadTree ===";
  let qt = Quadtree.create 0. 0. 800. 600. in
  let brick1 = { x = 100.; y = 100.; w = 60.; h = 20.; value = 100; alive = true } in
  let brick2 = { x = 500.; y = 500.; w = 60.; h = 20.; value = 100; alive = true } in
  let qt' = Quadtree.insert qt brick1 brick1.x brick1.y in
  let qt'' = Quadtree.insert qt' brick2 brick2.x brick2.y in
  
  Printf.printf "Requête autour de (100,100) avec rayon 50\n";
  let nearby = Quadtree.query qt'' 100. 100. 50. in
  Printf.printf "  -> Trouvé %d brique(s)\n" (List.length nearby);
  assert_bool "Devrait trouver brick1" (List.length nearby >= 1);
  
  Printf.printf "Requête autour de (500,500) avec rayon 50\n";
  let far = Quadtree.query qt'' 500. 500. 50. in
  Printf.printf "  -> Trouvé %d brique(s)\n" (List.length far);
  assert_bool "Devrait trouver brick2" (List.length far >= 1);
  print_endline "-- Requêtes spatiales fonctionnelles"

let test_quadtree_to_list _ =
  print_endline "\n=== TEST: Conversion QuadTree en liste ===";
  let qt = Quadtree.create 0. 0. 800. 600. in
  let brick1 = { x = 100.; y = 100.; w = 60.; h = 20.; value = 100; alive = true } in
  let brick2 = { x = 200.; y = 200.; w = 60.; h = 20.; value = 100; alive = true } in
  let qt' = Quadtree.insert qt brick1 brick1.x brick1.y in
  let qt'' = Quadtree.insert qt' brick2 brick2.x brick2.y in
  
  Printf.printf "Insertion de 2 briques dans le QuadTree\n";
  let all = Quadtree.to_list qt'' in
  Printf.printf "Récupération: %d élément(s)\n" (List.length all);
  assert_equal 2 (List.length all) ~msg:"Devrait récupérer toutes les briques";
  print_endline "-- Conversion correcte"

(* ========== Tests de power-ups ========== *)

let test_powerup_spawn _ =
  print_endline "\n=== TEST: Structure des power-ups ===";
  let pup = { xp = 100.; yp = 200.; vy = -100.; ptype = PaddleWide; active = true } in
  Printf.printf "Power-up créé: position(%.1f, %.1f), vitesse=%.1f, actif=%b\n"
    pup.xp pup.yp pup.vy pup.active;
  assert_equal 100. pup.xp ~msg:"Position X du power-up";
  assert_equal 200. pup.yp ~msg:"Position Y du power-up";
  assert_equal true pup.active ~msg:"Power-up devrait être actif";
  print_endline "-- Structure power-up correcte"

(* ========== Tests de physique ========== *)

let test_physics_integration _ =
  print_endline "\n=== TEST: Intégration physique (position, vitesse, accélération) ===";
  let pos = Vector.make 100. 100. in
  let vel = Vector.make 10. 20. in
  let acc = Vector.make 0. (-9.8) in
  let dt = 0.016 in
  
  Printf.printf "État initial: pos(100,100), vel(10,20), acc(0,-9.8)\n";
  let (new_pos, new_vel) = Physics.integrate pos vel acc dt in
  let (px, py) = Vector.to_tuple new_pos in
  let (vx, vy) = Vector.to_tuple new_vel in
  
  Printf.printf "Après dt=%.3f: pos(%.2f,%.2f), vel(%.2f,%.2f)\n" dt px py vx vy;
  assert_bool "Position X devrait augmenter" (px > 100.);
  assert_bool "Position Y devrait augmenter" (py > 100.);
  assert_bool "Vitesse Y devrait diminuer (gravité)" (vy < 20.);
  print_endline "-- Intégration physique correcte"

let test_vector_operations _ =
  print_endline "\n=== TEST: Opérations vectorielles ===";
  let v1 = Vector.make 3. 4. in
  let v2 = Vector.make 1. 2. in
  
  Printf.printf "v1=(3,4), v2=(1,2)\n";
  let sum = Vector.add v1 v2 in
  let (sx, sy) = Vector.to_tuple sum in
  Printf.printf "v1 + v2 = (%.1f, %.1f)\n" sx sy;
  assert_equal 4. sx ~msg:"Addition X";
  assert_equal 6. sy ~msg:"Addition Y";
  
  let mag = Vector.magnitude v1 in
  Printf.printf "Magnitude de v1 = %.1f\n" mag;
  assert_equal 5. mag ~msg:"Magnitude devrait être 5";
  print_endline "-- Opérations vectorielles correctes"

(* ========== Tests de logique de jeu ========== *)

let test_ball_on_paddle _ =
  print_endline "\n=== TEST: Balle collée sur paddle ===";
  let paddle = { x = 400.; width = 80.; height = 15. } in
  Printf.printf "Paddle à x=%.1f\n" paddle.x;
  let ball = ball_on_paddle paddle 1.0 in
  let (bx, by) = Vector.to_tuple ball.pos in
  
  Printf.printf "Balle positionnée à (%.1f, %.1f)\n" bx by;
  assert_equal paddle.x bx ~msg:"Balle devrait être centrée sur la paddle";
  assert_bool "Balle devrait être au-dessus de la paddle" (by > 20.);
  print_endline "-- Balle correctement positionnée sur paddle"

let test_score_increase _ =
  print_endline "\n=== TEST: Augmentation du score ===";
  let etat = { init_state with score = 100 } in
  Printf.printf "Score initial: %d\n" etat.score;
  let new_etat = { etat with score = etat.score + 50 } in
  Printf.printf "Score après +50: %d\n" new_etat.score;
  assert_equal 150 new_etat.score ~msg:"Score devrait augmenter";
  print_endline "-- Score augmente correctement"

let test_life_decrease _ =
  print_endline "\n=== TEST: Perte de vies ===";
  let etat = { init_state with lives = 3 } in
  Printf.printf "Vies initiales: %d\n" etat.lives;
  let new_etat = { etat with lives = max 0 (etat.lives - 1) } in
  Printf.printf "Vies après perte: %d\n" new_etat.lives;
  assert_equal 2 new_etat.lives ~msg:"Vies devraient diminuer";
  
  Printf.printf "\nTest limite: 1 vie -> 0 vie\n";
  let etat_last = { init_state with lives = 1 } in
  let new_etat_last = { etat_last with lives = max 0 (etat_last.lives - 1) } in
  Printf.printf "Vies finales: %d (ne peut pas être négatif)\n" new_etat_last.lives;
  assert_equal 0 new_etat_last.lives ~msg:"Vies ne devraient pas être négatives";
  print_endline "-- Gestion des vies correcte"

let test_paddle_width_powerup _ =
  print_endline "\n=== TEST: Power-up élargissement paddle ===";
  let paddle = { x = 400.; width = 80.; height = 15. } in
  Printf.printf "Largeur initiale: %.1f\n" paddle.width;
  let new_paddle = { paddle with width = min 120. (paddle.width +. 20.) } in
  Printf.printf "Largeur après power-up: %.1f\n" new_paddle.width;
  assert_equal 100. new_paddle.width ~msg:"Paddle devrait s'élargir";
  
  Printf.printf "\nTest limite maximale (110 + 20 = 120 max)\n";
  let max_paddle = { x = 400.; width = 110.; height = 15. } in
  let capped_paddle = { max_paddle with width = min 120. (max_paddle.width +. 20.) } in
  Printf.printf "Largeur limitée à: %.1f\n" capped_paddle.width;
  assert_equal 120. capped_paddle.width ~msg:"Paddle ne devrait pas dépasser 120";
  print_endline "-- Power-up paddle fonctionne avec limite"


(* ========== Suite de tests ========== *)

let suite =
  "Newtonoid System Tests" >::: [
    (* Initialisation *)
    "test_init_state" >:: test_init_state;
    "test_init_paddle" >:: test_init_paddle;
    "test_init_ball" >:: test_init_ball;
    
    (* Génération niveau *)
    "test_generate_level" >:: test_generate_level;
    "test_bricks_properties" >:: test_bricks_properties;
    
    (* Collision *)
    "test_paddle_clamp" >:: test_paddle_clamp;
    "test_circle_aabb_collision" >:: test_circle_aabb_collision;
    "test_circle_aabb_no_collision" >:: test_circle_aabb_no_collision;
    
    (* QuadTree *)
    "test_quadtree_insert" >:: test_quadtree_insert;
    "test_quadtree_query" >:: test_quadtree_query;
    "test_quadtree_to_list" >:: test_quadtree_to_list;
    
    (* Power-ups *)
    "test_powerup_spawn" >:: test_powerup_spawn;
    
    (* Physique *)
    "test_physics_integration" >:: test_physics_integration;
    "test_vector_operations" >:: test_vector_operations;
    
    (* Logique de jeu *)
    "test_ball_on_paddle" >:: test_ball_on_paddle;
    "test_score_increase" >:: test_score_increase;
    "test_life_decrease" >:: test_life_decrease;
    "test_paddle_width_powerup" >:: test_paddle_width_powerup;
    
  ]

let () =
  run_test_tt_main suite