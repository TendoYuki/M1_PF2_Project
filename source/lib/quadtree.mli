(* Interface du module QuadTree - Structure spatiale efficace *)

(* Type abstrait du QuadTree *)
type 'a t

(* Créer un QuadTree vide pour une zone donnée *)
(* Paramètres : x_min, y_min, x_max, y_max *)
val create : float -> float -> float -> float -> 'a t

(* Insérer un élément dans le QuadTree *)
(* Paramètres : quadtree, élément, x, y *)
val insert : 'a t -> 'a -> float -> float -> 'a t

(* Récupérer les éléments proches d'une position *)
(* Paramètres : quadtree, x, y, rayon *)
val query : 'a t -> float -> float -> float -> 'a list

(* Convertir le QuadTree en liste *)
val to_list : 'a t -> 'a list

(* Nombre d'éléments dans le QuadTree *)
val size : 'a t -> int