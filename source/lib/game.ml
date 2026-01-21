open Iterator

(* Initialisation du générateur aléatoire *)
let () = Random.self_init ()

(* --- Types du jeu --- *)

type vec = float * float


(* La balle *)
type ball = {
  pos : vec;
  vel : vec;
  acc : vec; 
  radius : float;
}

(* La raquette, on appelle la raquette, la barre qui bouge avec notre souris *)
type paddle = {
  x : float;
  width : float;
  height : float;
}

(* Les briques à casser *)
type brick = {
  x : float;
  y : float;
  w : float;
  h : float;
  value : int;
  alive : bool;
}


type etat = {
  ball : ball;
  paddle : paddle;
  bricks : brick list;
  score : int;
  lives : int;
  stuck : bool;          (* balle collée à la raquette *)
  (* 
    état précédent de la touche espace, 
    PS : c'est pour empecher d'appuyer 20 fois sur espace et 
    que le jeu relance la balle sans qu'on le veuille
  *)
  pressed_space : bool;  
  current_speed : float; (* facteur multiplicatif *)
  }

(* --- Constantes du jeu --- *)

let dt = 1. /. 60. (* la meme que sur le fichier newtonoid.ml *)

let box_infx = 10.
let box_infy = 10.
let box_supx = 790.
let box_supy = 590.

(* Gravité*)
let gravity = (0., -20.)

(* Facteur accélération constante de la balle au fil du temps *)
let ball_acceleration = 1.015

(* --- Initialisation --- *)

let init_ball = {
  pos = (400., 300.);
  vel = (120., 200.);
  acc = gravity;
  radius = 8.;
}

let init_paddle = {
  x = 400.;
  width = 80.;
  height = 15.;
}

let init_bricks =
  let rec rows y acc =
    if y > 550. then acc
    else
      let rec cols x acc2 =
        if x > 700. then acc2
        else
          let b = { x; y; w = 60.; h = 20.; value = 100; alive = true } in
          cols (x +. 70.) (b :: acc2)
      in
      rows (y +. 30.) (cols 50. acc)
  in
  rows 300. []


(* Mettre la balle collée à la raquette au début *)
let ball_on_paddle paddle speed =
  let py = 20. +. paddle.height +. 10. in
  let px = paddle.x in
  (* Direction aléatoire *)
  let direction = if Random.bool () then 1. else -1. in
  (* Vitesse augmente avec speed *)
  let vx = 120. *. speed *. direction in
  let vy = 200. *. speed in
  { pos = (px, py); 
    vel = (vx, vy);  
    acc = gravity; 
    radius = 8. 
  }

(* Etat de départ *)
let init_state = {
  ball = ball_on_paddle init_paddle 1.0;
  paddle = init_paddle;
  bricks = init_bricks;
  score = 0;
  lives = 3;
  stuck = true;
  pressed_space = false;
  current_speed = 1.0; 
}

(* Intégration : calcule position et vitesse à partir de l'accélération *)
let integrate (pos_x, pos_y) (vel_x, vel_y) (acc_x, acc_y) dt =
  (* Nouvelle vitesse : v' = v + a * dt *)
  let vel_x' = vel_x +. acc_x *. dt in
  let vel_y' = vel_y +. acc_y *. dt in
  (* Nouvelle position : p' = p + v' * dt *)
  let pos_x' = pos_x +. vel_x' *. dt in
  let pos_y' = pos_y +. vel_y' *. dt in
  ((pos_x', pos_y'), (vel_x', vel_y'))

(* --- Utilitaires --- *)
(* 
  Petit bout de code qui coince la valeur x entre a et b 
  Utile pour notre raquette pour qu'elle ne sorte pas de l'écran
*)
let clamp x a b =
  if x < a then a else if x > b then b else x

(* Détection collision cercle-rectangle *)
let circle_aabb_collision (cx, cy) radius (bx, by, bw, bh) =
  let closest_x = clamp cx bx (bx +. bw) in
  let closest_y = clamp cy by (by +. bh) in
  let dx = cx -. closest_x in
  let dy = cy -. closest_y in
  let distance = sqrt (dx *. dx +. dy *. dy) in
  distance < radius
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

  (* Détection propre de l'appui sur espace *)
  let space_now =
    Graphics.key_pressed () && Graphics.read_key () = ' '
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
    
    let (new_pos, new_vel) = 
      integrate ball.pos ball.vel ball.acc dt
    in
    
    let (nx, ny) = new_pos in
    let (vx, vy) = new_vel in
    
    (* Gestion des rebonds sur les murs latéraux *)
    let nx, vx, acc_x =
      if nx -. ball.radius < box_infx then
        (* Rebond mur gauche *)
        let vx' = -. vx *. ball_acceleration in  
        (box_infx +. ball.radius, vx', fst ball.acc)
      else if nx +. ball.radius > box_supx then
        (* Rebond mur droit *)
        let vx' = -. vx *. ball_acceleration in  
        (box_supx -. ball.radius, vx', fst ball.acc)
      else 
        (nx, vx, fst ball.acc)
    in

    (* Gestion du rebond sur le plafond *)
    let ny, vy, acc_y =
      if ny +. ball.radius > box_supy then

        let vy' = -. vy *. ball_acceleration in  
        (box_supy -. ball.radius, vy', snd ball.acc)
      else 
        (ny, vy, snd ball.acc)
    in
        
    (* Gestion du rebond sur la raquette *)
    let paddle_top = 20. +. paddle.height in
    let paddle_left = paddle.x -. paddle.width /. 2. in
    let paddle_right = paddle.x +. paddle.width /. 2. in

    let ny, vy, acc_y =
      (*balle touche dessus de raquette *)
      if ny -. ball.radius <= paddle_top 
        && ny -. ball.radius >= 20.
        && nx >= paddle_left 
        && nx <= paddle_right
        && vy < 0.  (* La balle descend *)
      then
        let vy' = -. vy *. ball_acceleration in  (* Inverse + accélère *)
        (paddle_top +. ball.radius, vy', acc_y)  (* Repositionne sur la raquette *)
      else
        (ny, vy, acc_y)
    in

    (* Collision avec briques *)
    let hit_brick = ref false in
    let bricks', score_bonus = 
      List.fold_left (fun (bricks_acc, score_acc) brick ->
        if brick.alive && circle_aabb_collision (nx, ny) ball.radius 
                          (brick.x, brick.y, brick.w, brick.h) then
          begin
            hit_brick := true;
            let dead_brick = { brick with alive = false } in
            (dead_brick :: bricks_acc, score_acc + brick.value)
          end
        else
          (brick :: bricks_acc, score_acc)
      ) ([], 0) etat.bricks
    in
    let bricks' = List.rev bricks' in
    let vy = if !hit_brick then -. vy *. ball_acceleration else vy in

    (* Si la balle tombe en dessous du sol : perte de vie *)
    if ny -. ball.radius < box_infy then
      { etat with
        ball = ball_on_paddle paddle 1.0; 
        paddle;
        lives = max 0 (etat.lives - 1);
        stuck = true;
        pressed_space = space_now;
        current_speed = 1.0;  
      }
    else
      (* Calculer la magnitude après les rebonds *)
      let speed_magnitude = sqrt (vx *. vx +. vy *. vy) in
      let initial_speed = sqrt (120. *. 120. +. 200. *. 200.) in
      let new_speed = speed_magnitude /. initial_speed in
      { etat with
        ball = { ball with pos = (nx, ny); vel = (vx, vy); acc = (acc_x, acc_y) };
        paddle;
        pressed_space = space_now;
        current_speed = new_speed;
        bricks = bricks';
        score = etat.score + score_bonus;
      }

(* --- Flux d'états --- *)

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


