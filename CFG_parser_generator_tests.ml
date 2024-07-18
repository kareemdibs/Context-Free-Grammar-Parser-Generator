type game_nonterminals =
  | Action
  | Character
  | Activity
  | Item
  | Conjunction
  | Hero        
  | Villain     
  | Companion   
  | Attack      
  | Use         
  | Move;;       

let game_grammar =
  (Action, function
    | Action -> [[N Character; N Activity; N Item; N Conjunction];
                [N Character; N Activity; N Item];
                [N Character; N Activity]]
    | Character -> [[N Hero];
                    [N Villain];
                    [N Companion]]
    | Activity -> [[N Attack];
                  [N Use];
                  [N Move]]
    | Item -> [[T "sword"];
              [T "shield"];
              [T "potion"]]
    | Hero -> [[T "Knight"];
              [T "Wizard"];
              [T "Archer"]]
    | Villain -> [[T "Dragon"];
                  [T "Orc"];
                  [T "Goblin"]]
    | Companion -> [[T "Elf"];
                    [T "Dwarf"];
                    [T "Hobbit"]]
    | Attack -> [[T "slashes"];
                [T "shoots"];
                [T "strikes"]]
    | Use -> [[T "uses"];
              [T "activates"];
              [T "employs"]]
    | Move -> [[T "moves to"];
              [T "runs to"];
              [T "teleports to"]]
    | Conjunction -> [[T "and then"];
                      [T "following"];
                      [T "subsequently"]]
  );;
 

let accept_all_strs string = Some string
let accept_empty = function
    | _::_ -> None
    | x -> Some x

let make_matcher_test0 = make_matcher game_grammar accept_all_strs ["slashes"] = None;;
let make_matcher_test1 = make_matcher game_grammar accept_all_strs ["Knight"; "slashes"] = Some [];;
let make_matcher_test2 = make_matcher game_grammar accept_empty ["Knight"; "slashes"; "sword"; "Knight"] = None;;
let make_matcher_test = make_matcher_test0 && make_matcher_test1 && make_matcher_test2;;

let make_parser_test0 = make_parser game_grammar ["slashes"] = None;;
let make_parser_test1 = make_parser game_grammar ["Knight"; "slashes"; "sword"; "Knight"] = None;;
let make_parser_test2 = make_parser game_grammar ["Dragon"] = None;;
let make_parser_test3 = make_parser game_grammar ["Knight"; "slashes"] = Some (Node (Action,
                                                                                    [Node (Character, 
                                                                                            [Node (Hero, [Leaf "Knight"])]);
                                                                                      Node (Activity, 
                                                                                            [Node (Attack, [Leaf "slashes"])])]));;

let make_parser_test = make_parser_test0 && make_parser_test1 && make_parser_test2 && make_parser_test3;;