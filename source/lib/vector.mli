(* Interface du module Vector - Type abstrait pour vecteurs 2D *)

type t

(* Créer un vecteur à partir de deux coordonnées *)
val make : float -> float -> t

(* Obtenir la composante x *)
val x : t -> float

(* Obtenir la composante y *)
val y : t -> float

(* Ajouter deux vecteurs *)
val add : t -> t -> t

(* Soustraire deux vecteurs *)
val sub : t -> t -> t

(* Multiplier un vecteur par un scalaire *)
val scale : float -> t -> t

(* Calculer la norme (magnitude) d'un vecteur *)
val magnitude : t -> float

(* Convertir en tuple (pour compatibilité Graphics) *)
val to_tuple : t -> float * float

(* Créer depuis un tuple *)
val of_tuple : float * float -> t