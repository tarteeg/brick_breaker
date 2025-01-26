open Types

let create_quadtree x y width height = Leaf ([], Pair (x, y), Pair (width, height))

let rec retrieve_all quadtree =
  match quadtree with
  | Leaf (objects, _, _) -> objects
  | Node (nw, ne, sw, se, _, _) ->
      retrieve_all nw @ retrieve_all ne @ retrieve_all sw @ retrieve_all se


let rec insert brick qt =
  match qt with
  | Leaf (bricks, Pair (x, y), Pair (width, height)) ->
    if List.length bricks < 4 then
      Leaf (brick :: bricks, Pair (x, y), Pair (width, height))
    else
      let half_width = width /. 2. in
      let half_height = height /. 2. in
      let nw = create_quadtree x y half_width half_height in
      let ne = create_quadtree (x +. half_width) y half_width half_height in
      let sw = create_quadtree x (y +. half_height) half_width half_height in
      let se = create_quadtree (x +. half_width) (y +. half_height) half_width half_height in
      let node = Node (nw, ne, sw, se, Pair (x, y), Pair (width, height)) in
      List.fold_left (fun acc b -> insert b acc) node (brick :: bricks)
  | Node (nw, ne, sw, se, Pair (x, y), Pair (width, height)) ->
    let Brique (Pair (bx, by), _, _) = brick in
    let half_width = width /. 2. in
    let half_height = height /. 2. in
    if bx < x +. half_width then
      if by < y +. half_height then
        Node (insert brick nw, ne, sw, se, Pair (x, y), Pair (width, height))
      else
        Node (nw, ne, insert brick sw, se, Pair (x, y), Pair (width, height))
    else
      if by < y +. half_height then
        Node (nw, insert brick ne, sw, se, Pair (x, y), Pair (width, height))
      else
        Node (nw, ne, sw, insert brick se, Pair (x, y), Pair (width, height))

let rec query qt (Pair (bx, by)) radius =
  match qt with
  | Leaf (bricks, Pair (x, y), Pair (width, height)) ->
    List.filter (fun brick ->
      let Brique (Pair (rx, ry), Pair (rw, rh), _) = brick in
      bx +. radius >= rx && bx -. radius <= rx +. rw &&
      by +. radius >= ry && by -. radius <= ry +. rh
    ) bricks
  | Node (nw, ne, sw, se, Pair (x, y), Pair (width, height)) ->
    let half_width = width /. 2. in
    let half_height = height /. 2. in
    let in_nw = bx -. radius < x +. half_width && by -. radius < y +. half_height in
    let in_ne = bx +. radius >= x +. half_width && by -. radius < y +. half_height in
    let in_sw = bx -. radius < x +. half_width && by +. radius >= y +. half_height in
    let in_se = bx +. radius >= x +. half_width && by +. radius >= y +. half_height in
    let bricks = ref [] in
    if in_nw then bricks := !bricks @ query nw (Pair (bx, by)) radius;
    if in_ne then bricks := !bricks @ query ne (Pair (bx, by)) radius;
    if in_sw then bricks := !bricks @ query sw (Pair (bx, by)) radius;
    if in_se then bricks := !bricks @ query se (Pair (bx, by)) radius;
    !bricks

let rec update_quadtree qt updated_briques =
  match qt with
  | Leaf (bricks, Pair (x, y), Pair (width, height)) ->
    let updated_bricks = List.map (fun brick ->
      match List.find_opt (fun (Brique (pos, _, _)) -> pos = (match brick with Brique (pos, _, _) -> pos)) updated_briques with
      | Some updated_brick -> updated_brick
      | None -> brick
    ) bricks in
    Leaf (updated_bricks, Pair (x, y), Pair (width, height))
  | Node (nw, ne, sw, se, Pair (x, y), Pair (width, height)) ->
    Node (
      update_quadtree nw updated_briques,
      update_quadtree ne updated_briques,
      update_quadtree sw updated_briques,
      update_quadtree se updated_briques,
      Pair (x, y),
      Pair (width, height)
    )