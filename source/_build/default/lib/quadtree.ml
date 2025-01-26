open Types

let create_quadtree x y width height = Leaf ([], Pair (x, y), Pair (width, height))

let rec retrieve_all quadtree =
  match quadtree with
  | Leaf (briques, _, _) -> briques
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
  | Leaf (bricks, _, _) ->
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
    let bricks_nw = if in_nw then query nw (Pair (bx, by)) radius else [] in
    let bricks_ne = if in_ne then query ne (Pair (bx, by)) radius else [] in
    let bricks_sw = if in_sw then query sw (Pair (bx, by)) radius else [] in
    let bricks_se = if in_se then query se (Pair (bx, by)) radius else [] in
    bricks_nw @ bricks_ne @ bricks_sw @ bricks_se

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

(* Tests unitaires *)
let%test "test_create_quadtree" =
  let qt = create_quadtree 0.0 0.0 100.0 100.0 in
  match qt with
  | Leaf (_, Pair (x, y), Pair (width, height)) ->
    x = 0.0 && y = 0.0 && width = 100.0 && height = 100.0
  | _ -> false

let%test "test_insert" =
  let qt = create_quadtree 0.0 0.0 100.0 100.0 in
  let brick = Brique (Pair (10.0, 10.0), Pair (20.0, 10.0), false) in
  let qt = insert brick qt in
  match qt with
  | Leaf (bricks, _, _) -> List.length bricks = 1
  | _ -> false

let%test "test_retrieve_all" =
  let qt = create_quadtree 0.0 0.0 100.0 100.0 in
  let brick1 = Brique (Pair (10.0, 10.0), Pair (20.0, 10.0), false) in
  let brick2 = Brique (Pair (30.0, 30.0), Pair (20.0, 10.0), false) in
  let qt = insert brick1 qt in
  let qt = insert brick2 qt in
  let bricks = retrieve_all qt in
  List.length bricks = 2

let%test "test_query" =
  let qt = create_quadtree 0.0 0.0 100.0 100.0 in
  let brick1 = Brique (Pair (10.0, 10.0), Pair (20.0, 10.0), false) in
  let brick2 = Brique (Pair (30.0, 30.0), Pair (20.0, 10.0), false) in
  let qt = insert brick1 qt in
  let qt = insert brick2 qt in
  let result = query qt (Pair (15.0, 15.0)) 5.0 in
  List.length result = 1 && List.hd result = brick1