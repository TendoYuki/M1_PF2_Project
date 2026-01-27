open Iterator

(* Initialisation du générateur aléatoire *)
let () = Random.self_init ()

open Types
open Vector
open Physics
open Collision
open Constants
open Quadtree

(* --- Initialisation --- *)

let init_ball = {
  pos = Vector.make 400. 300.;
  vel = Vector.make 120. 200.;
  acc = gravity;
  radius = 8.;
}

let init_paddle = {
  x = 400.;
  width = 80.;
  height = 15.;
}

(* Génère un niveau aléatoire de briques *)
let generate_random_level () =
  let qtree = Quadtree.create 0. 0. 800. 600. in
  let rec rows y qt =
    if y > 550. then qt
    else
      let rec cols x qt2 =
        if x > 700. then qt2
        else
          if Random.float 1.0 < 0.7 then
            let values = [| 50; 100; 150; 200 |] in
            let value = values.(Random.int 4) in
            let b = { x; y; w = 60.; h = 20.; value; alive = true } in
            cols (x +. 70.) (Quadtree.insert qt2 b x y)
          else
            cols (x +. 70.) qt2
      in
      rows (y +. 30.) (cols 50. qt)
  in
  rows 300. qtree

(* Mettre la balle collée à la raquette *)
let ball_on_paddle paddle speed =
  let py = 20. +. paddle.height +. 10. in
  let px = paddle.x in
  let direction = if Random.bool () then 1. else -1. in
  let vx = 120. *. speed *. direction in
  let vy = 200. *. speed in
  { pos = Vector.make px py; 
    vel = Vector.make vx vy;  
    acc = gravity; 
    radius = 8. 
  }

(* État de départ *)
let init_state = {
  ball = ball_on_paddle init_paddle 1.0;
  paddle = init_paddle;
  bricks = generate_random_level ();  
  score = 0;
  lives = 3;
  stuck = true;
  pressed_space = false;
  current_speed = 1.0;
  powerups = [];
}

(* --- Mise à jour du jeu (une frame) --- *)

let step etat =
  (* Mise à jour raquette *)
  let mouse_x = Graphics.mouse_pos () |> fst |> float_of_int in
  let paddle =
    { etat.paddle with
      x = clamp mouse_x (box_infx +. etat.paddle.width /. 2.)
                        (box_supx -. etat.paddle.width /. 2.)
    }
  in

  (* Détection appui espace - Version simplifiée *)
  let space_now = 
    if Graphics.key_pressed () then
      let c = Graphics.read_key () in
      c = ' '
    else
      false
  in

  let just_pressed_space =
    space_now && not etat.pressed_space
  in

  (* Si balle collée *)
  if etat.stuck then
    let ball = ball_on_paddle paddle etat.current_speed in  
    if just_pressed_space then
      { etat with ball; paddle; stuck = false; pressed_space = space_now }
    else
      { etat with ball; paddle; pressed_space = space_now }

  else
    let ball = etat.ball in
    
    (* Intégration physique *)
    let (new_pos, new_vel) = 
      integrate ball.pos ball.vel ball.acc dt
    in
    
    let nx = Vector.x new_pos in
    let ny = Vector.y new_pos in
    let vx = Vector.x new_vel in
    let vy = Vector.y new_vel in
    
    (* Rebonds murs latéraux *)
    let nx, vx, acc_x =
      if nx -. ball.radius < box_infx then
        let vx' = -. vx *. ball_acceleration in  
        (box_infx +. ball.radius, vx', Vector.x ball.acc)
      else if nx +. ball.radius > box_supx then
        let vx' = -. vx *. ball_acceleration in  
        (box_supx -. ball.radius, vx', Vector.x ball.acc)
      else 
        (nx, vx, Vector.x ball.acc)
    in

    (* Rebond plafond *)
    let ny, vy, acc_y =
      if ny +. ball.radius > box_supy then
        let vy' = -. vy *. ball_acceleration in  
        (box_supy -. ball.radius, vy', Vector.y ball.acc)
      else 
        (ny, vy, Vector.y ball.acc)
    in
        
    (* Rebond raquette *)
    let paddle_top = 20. +. paddle.height in
    let paddle_left = paddle.x -. paddle.width /. 2. in
    let paddle_right = paddle.x +. paddle.width /. 2. in

    let ny, vy, vx, acc_y =
      if ny -. ball.radius <= paddle_top 
        && ny -. ball.radius >= 20.
        && nx >= paddle_left 
        && nx <= paddle_right
        && vy < 0.
      then
        let hit_pos = nx -. paddle.x in
        let normalized = hit_pos /. (paddle.width /. 2.) in
        let spin_effect = normalized *. 100. in
        let vx' = vx +. spin_effect in
        let vy' = -. vy *. ball_acceleration in
        (paddle_top +. ball.radius, vy', vx', acc_y)
      else
        (ny, vy, vx, acc_y)
    in

    (* Collision avec briques - UTILISER QUADTREE *)
    let brick_was_hit = ref false in
    let nearby_bricks = Quadtree.query etat.bricks nx ny 100. in
    
    let (collision_result_bricks, score_bonus, spawned_powerups) : (brick list * int * powerup list) =
      List.fold_left (fun (bs, sc, ps) brick ->
        let ball_center = Vector.make nx ny in
        if brick.alive && circle_aabb ball_center ball.radius 
                          (brick.x, brick.y, brick.w, brick.h) then
          (
            brick_was_hit := true;
            let dead_brick = { brick with alive = false } in
            
            let maybe_pup = 
              if Random.float 1.0 < 0.2 then
                let types = [| PaddleWide; BallSlow; ExtraLife; PaddleNarrow |] in
                let t = types.(Random.int 4) in
                [{ xp = brick.x +. brick.w /. 2.;
                   yp = brick.y;
                   vy = -100.;
                   ptype = t;
                   active = true }]
              else
                []
            in
            
            (dead_brick :: bs, sc + brick.value, maybe_pup @ ps)
          )
        else
          (brick :: bs, sc, ps)
      ) ([], 0, []) nearby_bricks
    in
    
    print_endline "Début reconstruction QuadTree..."; flush stdout;
    (* Reconstruire le QuadTree SEULEMENT si brique cassée *)
    let new_qtree = 
      if !brick_was_hit then begin
        print_endline "Brique cassée, reconstruction..."; flush stdout;
        let all_existing_bricks = Quadtree.to_list etat.bricks in
        print_endline (Printf.sprintf "Total briques existantes: %d" (List.length all_existing_bricks)); flush stdout;
        
        let all_updated_bricks = 
          List.map (fun orig_brick ->
            match List.find_opt 
              (fun collision_brick -> collision_brick.x = orig_brick.x && collision_brick.y = orig_brick.y) 
              collision_result_bricks 
            with
            | Some found_brick -> found_brick
            | None -> orig_brick
          ) all_existing_bricks
        in
        print_endline (Printf.sprintf "Briques après mise à jour: %d" (List.length all_updated_bricks)); flush stdout;
        
        print_endline "Début création nouveau QuadTree..."; flush stdout;
        let qt = List.fold_left (fun qt brick ->
          Quadtree.insert qt brick brick.x brick.y
        ) (Quadtree.create 0. 0. 800. 600.) all_updated_bricks
        in
        print_endline "Nouveau QuadTree créé !"; flush stdout;
        qt
      end else begin
        print_endline "Pas de brique cassée, QuadTree inchangé"; flush stdout;
        etat.bricks
      end
    in
    print_endline "Fin reconstruction QuadTree"; flush stdout;

    (* Mise à jour power-ups - étape 1: déplacer *)
    let powerups_moved = 
      List.map (fun pup ->
        if pup.active then
          let yp' = pup.yp +. pup.vy *. dt in
          { pup with yp = yp' }
        else
          pup
      ) (etat.powerups @ spawned_powerups)
    in

    (* Mise à jour power-ups - étape 2: filtrer hors écran *)
    let powerups_filtered = 
      List.filter (fun pup -> pup.yp > 0.) powerups_moved
    in

    (* Collision power-up avec paddle *)
    let paddle_left = paddle.x -. paddle.width /. 2. in
    let paddle_right = paddle.x +. paddle.width /. 2. in
    let paddle_top = 20. +. paddle.height in

    let caught_powerup = ref None in

    let powerups_checked = 
      List.map (fun pup ->
        if pup.active 
           && pup.yp <= paddle_top 
           && pup.yp >= 20.
           && pup.xp >= paddle_left 
           && pup.xp <= paddle_right 
        then (
          caught_powerup := Some pup.ptype;
          { pup with active = false }
        ) else
          pup
      ) powerups_filtered
    in

    (* Appliquer effet power-up *)
    let paddle, current_speed, lives = 
      match !caught_powerup with
      | Some PaddleWide ->
          ({ paddle with width = min 120. (paddle.width +. 20.) }, 
           etat.current_speed, 
           etat.lives)
      
      | Some BallSlow ->
          (paddle, max 0.7 (etat.current_speed *. 0.8), etat.lives)
      
      | Some ExtraLife ->
          (paddle, etat.current_speed, etat.lives + 1)
      
      | Some PaddleNarrow ->
          ({ paddle with width = max 40. (paddle.width -. 20.) }, 
           etat.current_speed, 
           etat.lives)
      
      | None ->
          (paddle, etat.current_speed, etat.lives)
    in

    let powerups_final = List.filter (fun pup -> pup.active) powerups_checked in

    let vy = if !brick_was_hit then -. vy *. ball_acceleration else vy in

    (* Perte de vie *)
    if ny -. ball.radius < box_infy then begin
      { etat with
        ball = ball_on_paddle paddle 1.0; 
        paddle;
        lives = max 0 (etat.lives - 1);
        stuck = true;
        pressed_space = true;
        current_speed = 1.0;  
      }
    end
    else begin
      let speed_magnitude = sqrt (vx *. vx +. vy *. vy) in
      let initial_speed = sqrt (120. *. 120. +. 200. *. 200.) in
      let new_speed = speed_magnitude /. initial_speed in
      
      let all_destroyed = List.for_all (fun b -> not b.alive) (Quadtree.to_list new_qtree) in
      
      if all_destroyed then
        { etat with
          ball = { ball with 
                   pos = Vector.make nx ny; 
                   vel = Vector.make vx vy; 
                   acc = Vector.make acc_x acc_y };
          paddle;
          pressed_space = space_now;
          current_speed = new_speed;
          bricks = generate_random_level ();  
          score = etat.score + score_bonus + 500;  
        }
      else
        { etat with
          ball = { ball with 
                   pos = Vector.make nx ny; 
                   vel = Vector.make vx vy; 
                   acc = Vector.make acc_x acc_y };
          paddle;  
          pressed_space = space_now;
          current_speed;  
          bricks = new_qtree;
          score = etat.score + score_bonus;
          lives;  
          powerups = powerups_final;
        }
    end

(* Flux d'états *)
let flux_etat etat0 =
  Flux.unfold
    (fun etat ->
       if etat.lives <= 0 then
         None  
       else
         let etat' = step etat in
         Some (etat, etat'))
    etat0

let game_hello () =
  print_endline "Hello, Newtonoiders!"