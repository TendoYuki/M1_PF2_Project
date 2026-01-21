(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator

(* exemple d'ouvertue d'un tel module de la bibliotheque : *)
open Game

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
  let (bx, by) = etat.ball.pos in
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

  (* briques *)
  Graphics.set_color Graphics.green;
  List.iter (fun b ->
    if b.alive then
      Graphics.fill_rect (int_of_float b.x) (int_of_float b.y)
        (int_of_float b.w) (int_of_float b.h)
  ) etat.bricks

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
      loop flux_etat' (last_score + score etat)
    | _ -> assert false
  in
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;
  let score = loop flux_etat 0 in
  Format.printf "Score final : %d@\n" score;
  Graphics.close_graph ()

(* LIGNE FINALE : on lance réellement le jeu *)
let () =
  let flux = flux_etat init_state in
  draw flux