(* Module QuadTree - Structure spatiale efficace *)

(* Capacité maximale d'une feuille avant subdivision *)
let max_capacity = 10

(* Type du QuadTree *)
type 'a t =
  | Empty of bounds
  | Leaf of bounds * ('a * float * float) list
  | Node of {
      bounds : bounds;
      nw : 'a t;
      ne : 'a t;
      sw : 'a t;
      se : 'a t;
    }

and bounds = {
  x_min : float;
  y_min : float;
  x_max : float;
  y_max : float;
}

(* Créer un QuadTree vide *)
let create x_min y_min x_max y_max =
  Empty { x_min; y_min; x_max; y_max }

(* Vérifier si un point est dans les limites *)
let contains bounds x y =
  x >= bounds.x_min && x <= bounds.x_max &&
  y >= bounds.y_min && y <= bounds.y_max

(* Subdiviser une zone en 4 quadrants *)
let subdivide bounds =
  let mid_x = (bounds.x_min +. bounds.x_max) /. 2. in
  let mid_y = (bounds.y_min +. bounds.y_max) /. 2. in
  
  let nw = { x_min = bounds.x_min; y_min = mid_y; x_max = mid_x; y_max = bounds.y_max } in
  let ne = { x_min = mid_x; y_min = mid_y; x_max = bounds.x_max; y_max = bounds.y_max } in
  let sw = { x_min = bounds.x_min; y_min = bounds.y_min; x_max = mid_x; y_max = mid_y } in
  let se = { x_min = mid_x; y_min = bounds.y_min; x_max = bounds.x_max; y_max = mid_y } in
  
  (nw, ne, sw, se)

(* Insérer un élément *)
let rec insert tree elem x y =
  match tree with
  | Empty bounds ->
      if contains bounds x y then
        Leaf (bounds, [(elem, x, y)])
      else
        tree
  
  | Leaf (bounds, elems) ->
      if not (contains bounds x y) then
        tree
      else if List.length elems < max_capacity then
        Leaf (bounds, (elem, x, y) :: elems)
      else begin
        (* Subdivision nécessaire *)
        let (nw_bounds, ne_bounds, sw_bounds, se_bounds) = subdivide bounds in
        
        (* Créer les 4 quadrants vides *)
        let nw = Empty nw_bounds in
        let ne = Empty ne_bounds in
        let sw = Empty sw_bounds in
        let se = Empty se_bounds in
        
        (* Réinsérer tous les anciens éléments dans les bons quadrants *)
        let nw, ne, sw, se = 
          List.fold_left (fun (nw_acc, ne_acc, sw_acc, se_acc) (e, ex, ey) ->
            if contains nw_bounds ex ey then
              (insert nw_acc e ex ey, ne_acc, sw_acc, se_acc)
            else if contains ne_bounds ex ey then
              (nw_acc, insert ne_acc e ex ey, sw_acc, se_acc)
            else if contains sw_bounds ex ey then
              (nw_acc, ne_acc, insert sw_acc e ex ey, se_acc)
            else if contains se_bounds ex ey then
              (nw_acc, ne_acc, sw_acc, insert se_acc e ex ey)
            else
              (nw_acc, ne_acc, sw_acc, se_acc)  (* Ne devrait jamais arriver *)
          ) (nw, ne, sw, se) elems
        in
        
        (* Insérer le nouvel élément *)
        let nw, ne, sw, se =
          if contains nw_bounds x y then
            (insert nw elem x y, ne, sw, se)
          else if contains ne_bounds x y then
            (nw, insert ne elem x y, sw, se)
          else if contains sw_bounds x y then
            (nw, ne, insert sw elem x y, se)
          else if contains se_bounds x y then
            (nw, ne, sw, insert se elem x y)
          else
            (nw, ne, sw, se)
        in
        
        Node { bounds; nw; ne; sw; se }
      end
  
  | Node { bounds; nw; ne; sw; se } ->
      if not (contains bounds x y) then
        tree
      else
        let (nw_bounds, ne_bounds, sw_bounds, se_bounds) = subdivide bounds in
        if contains nw_bounds x y then
          Node { bounds; nw = insert nw elem x y; ne; sw; se }
        else if contains ne_bounds x y then
          Node { bounds; nw; ne = insert ne elem x y; sw; se }
        else if contains sw_bounds x y then
          Node { bounds; nw; ne; sw = insert sw elem x y; se }
        else if contains se_bounds x y then
          Node { bounds; nw; ne; sw; se = insert se elem x y }
        else
          tree

(* Requête pour récupérer les éléments proches *)
let rec query tree x y radius =
  let intersects bounds =
    let closest_x = max bounds.x_min (min x bounds.x_max) in
    let closest_y = max bounds.y_min (min y bounds.y_max) in
    let dx = x -. closest_x in
    let dy = y -. closest_y in
    sqrt (dx *. dx +. dy *. dy) <= radius
  in
  
  match tree with
  | Empty _ -> []
  | Leaf (bounds, elems) ->
      if intersects bounds then List.map (fun (e, _, _) -> e) elems else []
  | Node { bounds; nw; ne; sw; se } ->
      if intersects bounds then
        query nw x y radius @
        query ne x y radius @
        query sw x y radius @
        query se x y radius
      else
        []

(* Convertir en liste *)
let rec to_list tree =
  match tree with
  | Empty _ -> []
  | Leaf (_, elems) -> List.map (fun (e, _, _) -> e) elems
  | Node { nw; ne; sw; se; _ } ->
      to_list nw @ to_list ne @ to_list sw @ to_list se

(* Compter les éléments *)
let size tree = List.length (to_list tree)