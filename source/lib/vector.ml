(* Module Vector - Type concret pour vecteurs 2D *)


type t = {
  x : float;
  y : float;
}

(* Constructeur *)
let make x y = { x; y }

(* Accesseurs *)
let x v = v.x
let y v = v.y

(* Opérations *)
let add v1 v2 = { x = v1.x +. v2.x; y = v1.y +. v2.y }

let sub v1 v2 = { x = v1.x -. v2.x; y = v1.y -. v2.y }

let scale k v = { x = k *. v.x; y = k *. v.y }

let magnitude v = sqrt (v.x *. v.x +. v.y *. v.y)

(* Conversions *)
let to_tuple v = (v.x, v.y)

let of_tuple (x, y) = { x; y }