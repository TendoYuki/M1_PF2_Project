(* Interface du module Physics - Calculs physiques *)

(* Intégrer position et vitesse avec l'accélération *)
(* Paramètres : position, vitesse, accélération, dt *)
(* Retourne : (nouvelle_position, nouvelle_vitesse) *)
val integrate : Vector.t -> Vector.t -> Vector.t -> float -> Vector.t * Vector.t