let my_subset_test0 = not (subset [1;2;3;4] [1;2;3])
let my_subset_test1 = not (subset [1;2;] [])
let my_subset_test2 = subset [] []

let my_equal_sets_test0 = equal_sets [1;3;3] [1;3]
let my_equal_sets_test1 = not (equal_sets [1;5] [1;5;7])

let my_set_union_test0 = equal_sets (set_union [] [1;5;5;7]) [1;5;7]
let my_set_union_test1 = equal_sets (set_union [1;5] [2;5;6;7]) [1;1;2;5;6;7;7;6]

let my_set_intersection_test0 = equal_sets (set_intersection [] []) []
let my_set_intersection_test1 = equal_sets (set_intersection [1;2;3] [4;5;6;]) []

let my_set_diff_test0 = equal_sets (set_diff [1;2;3;] [4;5;6]) [1;2;3;]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x/x) 1000 = 1;;

type tiny_nonterminals =
  | Lol | Lmao | Rofl | Hehe | Hoho 

let tiny_grammar =
  Lol,
  [Hoho, [N Hoho];
   Hehe, [T "lmfao"];
   Rofl, [N Hehe];
   Lmao, [T "xD"];
   Lmao, [T "hi"; N Rofl];
   Lol, [N Lmao; T ","]]
   
let my_tiny_test0 = 
	filter_reachable tiny_grammar = 
		(Lol,
		[
	   Hehe, [T "lmfao"];
	   Rofl, [N Hehe];
	   Lmao, [T "xD"];
	   Lmao, [T "hi"; N Rofl];
	   Lol, [N Lmao; T ","]])
