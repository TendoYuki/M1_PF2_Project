open Libnewtonoid
open Iterator
open Game
open Vector
open Types
open Quadtree

module Init = struct
  let dt = 1. /. 60. (* 60 Hz *)
end

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. Box.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. Box.marge) +. Box.supy -. Box.infy))

let draw_state etat =
  (* fond *)
  Graphics.set_color Graphics.black;
  Graphics.fill_rect 0 0 800 600;

  (* balle *)
  let ball_pos = Vector.to_tuple etat.ball.pos in
  let (bx, by) = ball_pos in
  Graphics.set_color Graphics.red;
  Graphics.fill_circle (int_of_float bx) (int_of_float by)
    (int_of_float etat.ball.radius);

  (* raquette *)
  let px = etat.paddle.x in
  let w = etat.paddle.width in
  let h = etat.paddle.height in
  Graphics.set_color Graphics.blue;
  Graphics.fill_rect (int_of_float (px -. w /. 2.)) 20
    (int_of_float w) (int_of_float h);

  (* briques - CONVERTIR LE QUADTREE EN LISTE *)
  let all_bricks = Quadtree.to_list etat.bricks in
  List.iter (fun b ->
    if b.alive then begin
      let color = match b.value with
        | 50 -> Graphics.green
        | 100 -> Graphics.yellow
        | 150 -> Graphics.red
        | 200 -> Graphics.magenta
        | _ -> Graphics.cyan
      in
      Graphics.set_color color;
      Graphics.fill_rect (int_of_float b.x) (int_of_float b.y)
        (int_of_float b.w) (int_of_float b.h)
    end
  ) all_bricks;

  (* Affichage score et vies *)
  Graphics.set_color Graphics.white;
  Graphics.moveto 20 570;
  Graphics.draw_string (Printf.sprintf "Score: %d    Vies: %d" etat.score etat.lives);

  (* Affichage power-ups *)
  List.iter (fun pup ->
    if pup.active then begin
      let color = match pup.ptype with
        | PaddleWide -> Graphics.cyan
        | BallSlow -> Graphics.blue
        | ExtraLife -> Graphics.yellow
        | PaddleNarrow -> Graphics.red
      in
      Graphics.set_color color;
      Graphics.fill_circle (int_of_float pup.xp) (int_of_float pup.yp) 8
    end
  ) etat.powerups

(* extrait le score courant d'un etat : *)
let score etat : int = etat.score

let draw flux_etat =
  let rec loop flux_etat last_score =
    match Flux.(uncons flux_etat) with
    | None -> last_score
    | Some (etat, flux_etat') ->
      Graphics.clear_graph ();
      draw_state etat;
      Graphics.synchronize ();
      Unix.sleepf Init.dt;
      loop flux_etat' (score etat)
  in
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;
  let final_score = loop flux_etat 0 in
  
  (* Écran Game Over *)
  Graphics.clear_graph ();
  Graphics.set_color Graphics.black;
  Graphics.fill_rect 0 0 800 600;
  
  Graphics.set_color Graphics.white;
  Graphics.moveto 250 350;
  Graphics.draw_string "GAME OVER";
  
  Graphics.moveto 200 300;
  Graphics.draw_string (Printf.sprintf "Score final : %d" final_score);
  
  Graphics.moveto 200 250;
  Graphics.draw_string "Appuyez sur R pour rejouer";
  
  Graphics.moveto 200 220;
  Graphics.draw_string "Appuyez sur Q pour quitter";
  
  Graphics.synchronize ();
  
  (* Attendre le choix du joueur *)
  let rec wait_choice () =
    if Graphics.key_pressed () then
      let key = Graphics.read_key () in
      match key with
      | 'r' | 'R' -> 
          Graphics.clear_graph ();
          true
      | 'q' | 'Q' -> 
          false
      | _ -> 
          Unix.sleepf 0.01;
          wait_choice ()
    else (
      Unix.sleepf 0.01;
      wait_choice ()
    )
  in
  
  let replay = wait_choice () in
  
  if not replay then (
    Format.printf "Score final : %d@\n" final_score;
    Graphics.close_graph ()
  );
  
  replay

(* on lance le jeu *)
let () =
  let rec game_loop () =
    let flux = flux_etat init_state in
    let replay = draw flux in
    if replay then game_loop ()
  in
  game_loop ()