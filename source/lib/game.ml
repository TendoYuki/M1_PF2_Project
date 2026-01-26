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

(* Type de power-up *)
type powerup_type =
  | PaddleWide      (* Raquette plus large - BONUS *)
  | BallSlow        (* Balle ralentit - BONUS *)
  | ExtraLife       (* +1 vie - BONUS *)
  | PaddleNarrow    (* Raquette rétrécit - MALUS *)

(* Un power-up qui tombe *)
type powerup = {
  x : float;           (* Position X *)
  y : float;           (* Position Y *)
  vy : float;          (* Vitesse chute *)
  ptype : powerup_type; (* Type de bonus *)
  active : bool;       (* Encore actif  *)
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
  powerups : powerup list;
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

(* Génère un niveau aléatoire de briques *)
let generate_random_level () =
  let rec rows y acc =
    if y > 550. then acc
    else
      let rec cols x acc2 =
        if x > 700. then acc2
        else
          (* 70% de chance de créer une brique *)
          if Random.float 1.0 < 0.7 then
            (* Valeur aléatoire*)
            let values = [| 50; 100; 150; 200 |] in
            let value = values.(Random.int 4) in
            let b = { x; y; w = 60.; h = 20.; value; alive = true } in
            cols (x +. 70.) (b :: acc2)
          else
            cols (x +. 70.) acc2
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
  bricks = generate_random_level ();  
  score = 0;
  lives = 3;
  stuck = true;
  pressed_space = false;
  current_speed = 1.0;
  powerups = [];
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

    let ny, vy, vx, acc_y =
      if ny -. ball.radius <= paddle_top 
        && ny -. ball.radius >= 20.
        && nx >= paddle_left 
        && nx <= paddle_right
        && vy < 0.
      then
        (* Calcul de l'effet selon où la balle touche *)
        let hit_pos = nx -. paddle.x in  (* Position par rapport au centre *)
        
        (* hit_pos entre -40 (extrême gauche) et +40 (extrême droite) *)
        (* Normaliser entre -1 et +1 *)
        let normalized = hit_pos /. (paddle.width /. 2.) in (* Pour que l'effet soit proportionnel peu importe la raquette *)
        
        (* Ajouter un effet horizontal *)
        let spin_effect = normalized *. 100. in  (* On convertit normalized en vitesse horizontale *)
        let vx' = vx +. spin_effect in
        
        let vy' = -. vy *. ball_acceleration in
        (paddle_top +. ball.radius, vy', vx', acc_y)
      else
        (ny, vy, vx, acc_y)
    in

    (* Collision avec briques *)
    let hit_brick = ref false in
    let bricks', score_bonus, new_powerups = 
    (* On applique le fold sur chaque brique*)
      List.fold_left (fun (bricks_acc, score_acc, pups_acc) brick ->
        (* Test collision *)
        if brick.alive && circle_aabb_collision (nx, ny) ball.radius 
                          (brick.x, brick.y, brick.w, brick.h) then
          (
            (* Marquer la brique comme morte*)
            hit_brick := true;
            let dead_brick = { brick with alive = false } in
            
            (* Générer un power-up 20% de chance *)
            let new_pup = 
              if Random.float 1.0 < 0.2 then
                (* Choisir un type de power-up aléatoire *)
                let ptypes = [| PaddleWide; BallSlow; ExtraLife; PaddleNarrow |] in
                let ptype = ptypes.(Random.int 4) in
                (* Créer le power-up*)
                [{ x = brick.x +. brick.w /. 2.;  (* Centre de la brique *)
                   y = brick.y;
                   vy = -100.;  (* Tombe à 100 pixels/s *)
                   ptype;
                   active = true }]
              else
                []
            in
            
            (dead_brick :: bricks_acc, score_acc + brick.value, new_pup @ pups_acc)
          )
        else
          (* Si pas de collision *)
          (brick :: bricks_acc, score_acc, pups_acc)
      ) ([], 0, []) etat.bricks
    in
    
    (* Mise à jour des power-ups *)
    let powerups' = 
      List.map (fun pup ->
        if pup.active then
          (* Le power-up tombe *)
          let y' = pup.y +. pup.vy *. dt in
          { pup with y = y' }
        else
          pup
      ) (etat.powerups @ new_powerups)  (* Ajouter les nouveaux *)
    in

    (* Filtrer ceux qui sont tombés hors de l'écran *)
    let powerups' = 
      List.filter (fun pup -> pup.y > 0.) powerups'
    in

    (* Collision power-up avec paddle *)
    let paddle_left = paddle.x -. paddle.width /. 2. in
    let paddle_right = paddle.x +. paddle.width /. 2. in
    let paddle_top = 20. +. paddle.height in

    let caught_powerup = ref None in

    let powerups' = 
      List.map (fun pup ->
        if pup.active 
           && pup.y <= paddle_top 
           && pup.y >= 20.
           && pup.x >= paddle_left 
           && pup.x <= paddle_right 
        then (
          caught_powerup := Some pup.ptype;  (* Attrapé ! *)
          { pup with active = false }
        ) else
          pup
      ) powerups'
    in

    (* Appliquer effet du power-up attrapé *)
    let paddle, current_speed, lives = 
      match !caught_powerup with
      | Some PaddleWide ->
          (* Élargir la raquette *)
          ({ paddle with width = min 120. (paddle.width +. 20.) }, 
           etat.current_speed, 
           etat.lives)
      
      | Some BallSlow ->
          (* Ralentir la balle *)
          (paddle, max 0.7 (etat.current_speed *. 0.8), etat.lives)
      
      | Some ExtraLife ->
          (* +1 vie *)
          (paddle, etat.current_speed, etat.lives + 1)
      
      | Some PaddleNarrow ->
          (* MALUS : Rétrécir la raquette *)
          ({ paddle with width = max 40. (paddle.width -. 20.) }, 
           etat.current_speed, 
           etat.lives)
      
      | None ->
          (* Pas de power-up attrapé *)
          (paddle, etat.current_speed, etat.lives)
    in

    (* Filtrer les inactifs *)
    let powerups' = List.filter (fun pup -> pup.active) powerups' in

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
      (* Calculer la vitesse actuelle de la balle pour la sauvegarder *)

      (* Étape 1 : Calculer la vitesse totale actuelle de la balle *)
      (* Formule : vitesse = √(vx² + vy²) - Théorème de Pythagore *)
      let speed_magnitude = sqrt (vx *. vx +. vy *. vy) in

      (* Étape 2 : Vitesse de référence (vitesse initiale au lancement) *)
      (* C'est la vitesse quand speed = 1.0, soit vx=120 et vy=200 *)
      let initial_speed = sqrt (120. *. 120. +. 200. *. 200.) in
      (* Étape 3 : Calculer le ratio (facteur multiplicatif) *)
      let new_speed = speed_magnitude /. initial_speed in
      
      (* Vérifier si toutes les briques sont cassées *)
      let all_destroyed = List.for_all (fun b -> not b.alive) bricks' in
      
      if all_destroyed then
        (* Nouveau niveau *)
        { etat with
          ball = { ball with pos = (nx, ny); vel = (vx, vy); acc = (acc_x, acc_y) };
          paddle;
          pressed_space = space_now;
          current_speed = new_speed;
          bricks = generate_random_level ();  
          score = etat.score + score_bonus + 500;  
        }
      else
        (* Mise à jour normale *)
        { etat with
          ball = { ball with pos = (nx, ny); vel = (vx, vy); acc = (acc_x, acc_y) };
          paddle;  
          pressed_space = space_now;
          current_speed;  
          bricks = bricks';
          score = etat.score + score_bonus;
          lives;  
          powerups = powerups';  
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