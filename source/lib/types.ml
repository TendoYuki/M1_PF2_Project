(* Types du jeu *)

(* La balle *)
type ball = {
  pos : Vector.t;
  vel : Vector.t;
  acc : Vector.t; 
  radius : float;
}

(* La raquette *)
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
  | PaddleWide
  | BallSlow
  | ExtraLife
  | PaddleNarrow

(* Un power-up  *)
type powerup = {
  xp : float;
  yp : float;
  vy : float;
  ptype : powerup_type;
  active : bool;
}

(* État complet du jeu *)
type etat = {
  ball : ball;
  paddle : paddle;
  bricks : brick Quadtree.t;
  score : int;
  lives : int;
  stuck : bool;
  pressed_space : bool;
  current_speed : float;
  powerups : powerup list;
}