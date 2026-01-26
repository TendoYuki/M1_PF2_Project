(* Module Collision - Détection de collisions *)

let clamp x a b =
  if x < a then a else if x > b then b else x

let circle_aabb center radius (bx, by, bw, bh) =
  let cx = Vector.x center in
  let cy = Vector.y center in
  
  (* Trouver le point le plus proche sur le rectangle *)
  let closest_x = clamp cx bx (bx +. bw) in
  let closest_y = clamp cy by (by +. bh) in
  
  (* Calculer la distance *)
  let dx = cx -. closest_x in
  let dy = cy -. closest_y in
  let distance = sqrt (dx *. dx +. dy *. dy) in
  
  (* Collision si distance < rayon *)
  distance < radius