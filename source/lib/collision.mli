(* Interface du module Collision *)

(* Limiter une valeur entre deux bornes *)
val clamp : float -> float -> float -> float

(* Détecter collision entre un cercle et un rectangle (AABB) *)
(* Paramètres : centre_cercle, rayon, (x, y, largeur, hauteur) du rectangle *)
val circle_aabb : Vector.t -> float -> (float * float * float * float) -> bool