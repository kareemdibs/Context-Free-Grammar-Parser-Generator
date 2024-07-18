type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

type ('nonterminal, 'terminal) symbol = 
  | N of 'nonterminal 
  | T of 'terminal

let convert_grammar gram1 = 
  let rec get_productions gram1 non_terminal = 
    let rec associate_productions prod_list = 
      match prod_list with
      | [] -> []
      | head::tail -> (snd head)::associate_productions tail
    in
    let rules = List.filter (fun p -> fst p = non_terminal) (snd gram1)
    in
      associate_productions rules
  in
  (fst gram1), get_productions gram1;;

let rec parse_tree_leaves tree = 
    let rec gather_leaves leaf_list =
      match leaf_list with
      | [] -> []
      | head::tail -> parse_tree_leaves head @ gather_leaves tail
    in
    match tree with
    | Node (_, head::tail) -> (parse_tree_leaves head) @ (gather_leaves tail)
    | Leaf x -> [x]
    | _ -> [];;

let make_matcher gram = 
  let rec match_nonterms right_hand_sides rule_map accept frag =
    let rec process_rhs rhs rule_map accept frag =
      let match_terms term accept frag =
        match frag with
        | [] -> None
        | prefix::suffix -> if prefix = term then accept suffix else None
      in
      match rhs with
      | [] -> accept frag
      | node::nodes -> 
          match node with
          | T x -> match_terms x (process_rhs nodes rule_map accept) frag
          | N x -> match frag with
                  | [] -> if rhs = [] then accept [] else None
                  | lst -> match_nonterms (rule_map x) rule_map (process_rhs nodes rule_map accept) lst
    in
    match right_hand_sides with
    | [] -> None
    | current_rhs::remaining_rhss -> match (process_rhs current_rhs rule_map accept frag) with
                                     | None -> match_nonterms remaining_rhss rule_map accept frag
                                     | Some x -> Some x
  in
  fun accept frag -> match_nonterms ((snd gram) (fst gram)) (snd gram) accept frag;;

let rec construct_tree path =
  let rec build_node curr path =
    match curr with
    | [] -> path, []
    | head::tail -> match head with
                    | T x -> (match build_node tail path with
                              | (remaining, node_level) -> remaining, (Leaf x)::node_level)
                    | N x -> match construct_tree path with
                              | (level, current_node) -> match build_node tail level with
                                                         | (remaining, child_nodes) -> remaining, current_node::child_nodes
  in
  match build_node (snd (List.hd path)) (List.tl path) with
   | (remaining, nodes_level) -> remaining, Node((fst (List.hd path)), nodes_level);;

let make_parser gram frag =
  let generate_path gram = 
    let rec parse_nonterms lhs right_hand_sides rule_map accept path frag =
      let rec parse_rhs rhs rule_map accept path frag =
        let parse_terms term accept frag =
          match frag with
          | [] -> None
          | prefix::suffix -> if prefix = term then accept suffix else None
        in
        match rhs with
        | [] -> accept path frag
        | node::nodes -> 
            match node with
            | T x -> parse_terms x (parse_rhs nodes rule_map accept path) frag
            | N x -> match frag with
                    | [] -> if rhs = [] then (accept path []) else None
                    | lst -> parse_nonterms x (rule_map x) rule_map (parse_rhs nodes rule_map accept) path lst
      in
      match right_hand_sides with
      | [] -> None
      | current_rhs::remaining_rhss -> match (parse_rhs current_rhs rule_map accept ((lhs, current_rhs)::path) frag) with
                                       | None -> parse_nonterms lhs remaining_rhss rule_map accept path frag
                                       | Some x -> Some x
    and accept_empty path = function
        | [] -> Some path
        | _ -> None
    in
    fun frag -> parse_nonterms (fst gram) ((snd gram) (fst gram)) (snd gram) accept_empty [] frag
  in
  match generate_path gram frag with
  | Some path -> Some (snd (construct_tree (List.rev path)))
  | None -> None;;