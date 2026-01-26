(* Constantes du jeu *)

(* Temps entre deux frames (60 FPS) *)
let dt = 1. /. 60.

(* Limites de la zone de jeu *)
let box_infx = 10.
let box_infy = 10.
let box_supx = 790.
let box_supy = 590.

(* Gravité (accélération vers le bas) *)
let gravity = Vector.make 0. (-20.)

(* Facteur d'accélération à chaque rebond *)
let ball_acceleration = 1.015