open Core;;

type action = Uncover | Flag
type position = {y: int; x: int}
type click = {p: position; a: action}

let position_init (y: int) (x: int) ~(height: int) ~(width: int): position option =
    if 0 <= y && y < height && 0 <= x && x < width
    then Some({y = y; x = x})
    else None

let create_click (p: position) (a: action): click =
    {p = p; a = a}

type cell_knowledge = Unknown | Uncovered | Flagged
type cell_state = {has_bomb: bool; state: cell_knowledge}
let cell_is_unknown cell = match cell with
                           | {has_bomb = _; state = Unknown} -> true
                           | _ -> false
type board = cell_state array array

let create_board (height: int) (width: int) (bombs: int): board =
    let generate_bomb_locations (bombs: int): (int * int) list =
        let generate_random_coordinates (): (int * int) =
            (Random.int height, Random.int width) in
        let rec helper (bombs_left: int) (coord_array: (int * int) list) =
            if
                bombs_left = 0
            then
                coord_array
            else
                let (x,y) = generate_random_coordinates () in
                if
                    List.exists coord_array ~f:(fun (a,b) -> a = x && b = y)
                then
                    helper bombs_left coord_array
                else
                    helper (bombs_left - 1) ((x,y) :: coord_array) in
        helper bombs [] in
    let bomb_locations = generate_bomb_locations bombs in
    Array.init height ~f:(fun i ->
        Array.init width ~f:(fun j ->
            let is_bomb_location = List.exists bomb_locations ~f:(fun (a,b) -> a = i && b = j) in
            {has_bomb = is_bomb_location; state=Unknown}
        )
    )

let board_lookup (b: board) (p: position): cell_state =
    let {y; x} = p in
    Array.get (Array.get b y) x

let board_uncover_all (b: board) =
    Array.map b ~f:(fun row ->
        Array.map row ~f:(fun cell -> match cell with
            | {has_bomb = has_bomb; state = Unknown} -> {has_bomb = has_bomb; state = Uncovered}
            | {has_bomb = has_bomb; state = Uncovered} -> {has_bomb = has_bomb; state = Uncovered}
            | {has_bomb = has_bomb; state = Flagged} -> {has_bomb = has_bomb; state = Flagged}
        )
    )

let board_uncover_position (b: board) (p: position): board =
    let {y; x} = p in
    let cell = board_lookup b p in
    let b_copy = Array.copy_matrix b in
    let () = b_copy.(y).(x) <- {has_bomb = cell.has_bomb; state = Uncovered} in
    b_copy

let board_flag_position (b: board) (p: position): board =
    let {y; x} = p in
    let cell = board_lookup b p in
    let b_copy = Array.copy_matrix b in
    let () = b_copy.(y).(x) <- {has_bomb = cell.has_bomb; state = Flagged} in
    b_copy

let board_apply_click (b: board) (c: click): board =
    match c with
    | {p; a=Uncover} -> board_uncover_position b p
    | {p; a=Flag} -> board_flag_position b p

let board_adjacent_positions (b: board) (p: position): position list =
    let is_position_in_board (p: position): bool =
        let board_y = Array.length b in
        let board_x = Array.length (Array.get b 0) in
        let {y; x} = p in
        0 <= y && y < board_y && 0 <= x && x < board_x in
    let {y; x} = p in
    let all_adjacents = [{y = y - 1; x = x    }; {y = y - 1; x = x - 1}; {y = y - 1; x = x + 1};
                         {y = y    ; x = x - 1};                         {y = y    ; x = x + 1};
                         {y = y + 1; x = x    }; {y = y + 1; x = x - 1}; {y = y + 1; x = x + 1}] in
    List.filter all_adjacents ~f:is_position_in_board

let board_all_positions (b: board): position list =
    let board_y = Array.length b in
    let board_x = Array.length (Array.get b 0) in
    List.init (board_y * board_x) ~f:(fun ij ->
        let i = ij / board_x in
        let j = ij mod board_x in
        {y = i; x = j}
    )

let board_count_bombs (b: board) =
    Array.fold b ~init:0 ~f:(fun s row ->
        s + Array.count row ~f:(fun cell -> match cell with
            | {has_bomb = true; state = Unknown} -> true
            | _ -> false
        )
    )

let board_to_string (b: board): string =
    let board_position_to_char (p: position): string =
        let {y; x} = p in
        let row = Array.get b y in
        let c = Array.get row x in
        match c with
        | {has_bomb = _; state = Unknown} -> "⬜"
        | {has_bomb = true; state = Uncovered} -> "⬛"
        | {has_bomb = false; state = Uncovered} ->
            let adjacents = board_adjacent_positions b p in
            let unknown_adjacents = List.filter adjacents ~f:(fun p -> (cell_is_unknown (board_lookup b p))) in
            let unknown_and_bomb_adjacents = List.filter unknown_adjacents ~f:(fun p -> (board_lookup b p).has_bomb) in
            let unknown_and_bomb_adjacents_count = List.length unknown_and_bomb_adjacents in
            (
                match unknown_and_bomb_adjacents_count with
                | 0 -> "🟩"
                | 1 -> "1️⃣"
                | 2 -> "2️⃣"
                | 3 -> "3️⃣"
                | 4 -> "4️⃣"
                | 5 -> "5️⃣"
                | 6 -> "6️⃣"
                | 7 -> "7️⃣"
                | 8 -> "8️⃣"
                | _ -> "🔺" (* TODO Raise error *)
            )
        | {has_bomb = _; state = Flagged} -> "🟥" in
    let rec helper (curr_y: int) (curr_x: int) (l: string list): string list =
        if curr_y = Array.length b
        then l
        else
            let row = Array.get b curr_y in
            if curr_x = Array.length row
            then helper (curr_y + 1) 0 ("\n" :: l)
            else
                let cell_char = board_position_to_char {y=curr_y; x=curr_x} in
                helper curr_y (curr_x + 1) (cell_char :: l) in
    let reverse_board = helper 0 0 [] in
    let b_as_str = List.rev reverse_board in
    String.concat b_as_str

let board_propagate (b: board): board =
    let rec helper (b: board) (cl: click list) (pl: position list): board = match cl with
    | [] -> (
            match pl with
            | [] -> b
            | p :: pt ->
                (* Propagation rules:
                    1. If this cell has 0 unflagged bombs next to it, then uncover adjacents and propagate
                    2. If this cell has 0 uncovered blank cells next to it, then flag adjacents and propagate *)
                let is_position_known = not (cell_is_unknown (board_lookup b p)) in
                let is_position_blank = not (board_lookup b p).has_bomb in
                let adjacents = board_adjacent_positions b p in
                let unknown_adjacents = List.filter adjacents ~f:(fun p -> (cell_is_unknown (board_lookup b p))) in
                let unknown_and_bomb_adjacents = List.filter unknown_adjacents ~f:(fun p -> (board_lookup b p).has_bomb) in
                let unknown_and_blank_adjacents = List.filter unknown_adjacents ~f:(fun p -> not (board_lookup b p).has_bomb) in
                if is_position_known && is_position_blank && List.length unknown_adjacents > 0 && List.length unknown_adjacents = List.length unknown_and_bomb_adjacents
                then
                    let propagated_clicks = List.map unknown_and_bomb_adjacents ~f:(fun p -> {p=p; a=Flag}) in
                    helper b propagated_clicks pt
                else if is_position_known && is_position_blank && List.length unknown_adjacents > 0 && List.length unknown_adjacents = List.length unknown_and_blank_adjacents
                then
                    let propagated_clicks = List.map unknown_and_blank_adjacents ~f:(fun p -> {p=p; a=Uncover}) in
                    helper b propagated_clicks pt
                else helper b [] pt
            )
    | c :: ct -> (
        let p = c.p in
        let first_adjacents = board_adjacent_positions b p in
        let second_adjacents = List.join (List.map first_adjacents ~f:(fun p -> board_adjacent_positions b p)) in
        helper (board_apply_click b c) ct (List.concat [first_adjacents; second_adjacents; pl])
    ) in
    let b_copy = Array.copy_matrix b in
    helper b_copy [] (board_all_positions b_copy)

let is_uniquely_solvable (b: board): bool =
    true

type game_state = Initial of int * int * int | Running of board | Won of board | Lost of board

exception CodeError of string

let state_to_string (g: game_state): string = match g with
    | Initial _ -> "No click yet!"
    | Running b ->
        let bombs_left = board_count_bombs b in
        (Printf.sprintf "Running, %d bombs left.\n" bombs_left) ^ board_to_string b
    | Won b -> "Won\n" ^ board_to_string b
    | Lost b -> "Lost\n" ^ board_to_string b

let rec game_state_transition (g: game_state) (c: click): game_state =
    let {p; a} = c in
    match g with
    | Initial (height, width, bombs) ->
        let board_with_first_good_click =
            let rec helper () =
                let b = create_board height width bombs in
                let post_click = game_state_transition (Running b) c
                match post_click with
                | Initial _ -> raise (CodeError "board reverted to Initial state")
                | Running b ->
                    if not is_uniquely_solvable b
                    then helper ()
                    else post_click
                | Won _ -> post_click
                | Lost _ -> helper () in
            helper () in
        board_with_first_good_click
    | Running b ->
        let clicked_cell = (board_lookup b p) in (
        match c.a with
        | Uncover -> if clicked_cell.has_bomb
                     then
                        let uncovered_b = board_uncover_all b in
                        Lost uncovered_b
                     else
                        let b_after_action = board_uncover_position b p in
                        let propagated_b = board_propagate b_after_action in
                        let bombs_remaining = board_count_bombs propagated_b in
                        if bombs_remaining = 0 then
                            let uncovered_b = board_uncover_all b in
                            Won uncovered_b
                        else
                            Running propagated_b
        | Flag -> if not clicked_cell.has_bomb
                  then
                      let uncovered_b = board_uncover_all b in
                      Lost uncovered_b
                  else
                      let b_after_action = board_flag_position b p in
                      let propagated_b = board_propagate b_after_action in
                      let bombs_remaining = board_count_bombs propagated_b in
                      if bombs_remaining = 0 then
                          let uncovered_b = board_uncover_all b in
                          Won uncovered_b
                      else
                          Running propagated_b
        )
    | Won _ -> g
    | Lost _ -> g

let () =
    let b = Initial (16, 30, 99) in
    let rec helper (g: game_state): unit =
        let (y, x, a) = Scanf.scanf "%d %d %s\n" (fun a b c -> (a, b, c)) in
        let c_opt = if String.equal a "uncover"
                    then Some {p={y=y; x=x}; a=Uncover}
                    else if String.equal a "flag"
                    then Some {p={y=y; x=x}; a=Flag}
                    else None in
        match c_opt with
        | None -> helper g
        | Some c ->
            let new_state = game_state_transition g c in
            let () = print_string (state_to_string new_state) in
            let () = Out_channel.flush stdout in
            helper new_state in
    helper b
