open Iterator

(* --- Types du jeu --- *)

type vec = float * float


(* La balle *)
type ball = {
  pos : vec;
  vel : vec;
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
  }

(* --- Constantes du jeu --- *)

let dt = 1. /. 60. (* la meme que sur le fichier newtonoid.ml *)

let box_infx = 10.
let box_infy = 10.
let box_supx = 790.
let box_supy = 590.

(* --- Initialisation --- *)

let init_ball = {
  pos = (400., 300.);
  vel = (120., 200.);
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
let ball_on_paddle paddle =
  let py = 20. +. paddle.height +. 10. in
  let px = paddle.x in
  { pos = (px, py); vel = (120., 200.); radius = 8. }

(* Etat de départ *)
let init_state = {
  ball = ball_on_paddle init_paddle;
  paddle = init_paddle;
  bricks = init_bricks;
  score = 0;
  lives = 3;
  stuck = true;
  pressed_space = false;
}

(* --- Utilitaires --- *)
(* 
  Petit bout de code qui coince la valeur x entre a et b 
  Utile pour notre raquette pour qu'elle ne sorte pas de l'écran
*)
let clamp x a b =
  if x < a then a else if x > b then b else x

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
    let ball = ball_on_paddle paddle in
    if just_pressed_space then
      { etat with ball; paddle; stuck = false; pressed_space = space_now }
    else
      { etat with ball; paddle; pressed_space = space_now }

  else
    (* Mouvement normal *)
    let (bx, by) = etat.ball.pos in
    let (vx, vy) = etat.ball.vel in

    let nx = bx +. vx *. dt in
    let ny = by +. vy *. dt in

    let nx, vx =
      if nx -. etat.ball.radius < box_infx then
        (box_infx +. etat.ball.radius, -. vx)
      else if nx +. etat.ball.radius > box_supx then
        (box_supx -. etat.ball.radius, -. vx)
      else (nx, vx)
    in

    let ny, vy =
      if ny +. etat.ball.radius > box_supy then
        (box_supy -. etat.ball.radius, -. vy)
      else (ny, vy)
    in

    if ny -. etat.ball.radius < box_infy then
      { etat with
        ball = ball_on_paddle paddle;
        paddle;
        lives = max 0 (etat.lives - 1);
        stuck = true;
        pressed_space = space_now;
      }
    else
      { etat with
        ball = { etat.ball with pos = (nx, ny); vel = (vx, vy) };
        paddle;
        pressed_space = space_now;
      }

(* --- Flux d'états --- *)

let flux_etat etat0 =
  Flux.unfold
    (fun etat ->
       let etat' = step etat in
       Some (etat, etat'))
    etat0

let game_hello () =
  print_endline "Hello, Newtonoiders!"