(* Module Physics - Calculs physiques *)

let integrate pos vel acc dt =
  (* Extraire les composantes *)
  let pos_x = Vector.x pos in
  let pos_y = Vector.y pos in
  let vel_x = Vector.x vel in
  let vel_y = Vector.y vel in
  let acc_x = Vector.x acc in
  let acc_y = Vector.y acc in
  
  (* Nouvelle vitesse : v' = v + a * dt *)
  let vel_x' = vel_x +. acc_x *. dt in
  let vel_y' = vel_y +. acc_y *. dt in
  
  (* Nouvelle position : p' = p + v' * dt *)
  let pos_x' = pos_x +. vel_x' *. dt in
  let pos_y' = pos_y +. vel_y' *. dt in
  
  (Vector.make pos_x' pos_y', Vector.make vel_x' vel_y')