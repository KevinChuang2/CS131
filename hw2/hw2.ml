type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
  
let rec convert_rules rules acc sym = match rules with
    | [] -> acc
    | hd::tl -> match hd with
        | (nt, lst) -> if nt = sym then convert_rules tl (acc@lst) sym
            else convert_rules tl acc sym
    
let convert_grammar g = match g with
	| (startSymbol, rules) -> (startSymbol, convert_rules rules [] )
    
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal
  
let rec parse_tree_helper tree = match tree with
    | [] -> []
    | [Leaf x] -> [Leaf x]
    | [Node (x, y)] -> (match y with
        | hd::tl -> parse_tree_helper [hd] @ parse_tree_helper tl
        | [] -> [])
    | hd::tl -> parse_tree_helper [hd] @ parse_tree_helper tl;;
    
let rec strip_leaf lst acc = match lst with
    | [] -> acc
    | hd::tl -> (match hd with 
        | Leaf x -> strip_leaf tl (acc@[x]))

let parse_tree_leaves tree = strip_leaf (parse_tree_helper [tree]) []

let rec match_nonterm startSym rules func acceptor acc frag = 
    match rules with
    (* If there are no more rules that can match the nonterminal, then we can't match fragment; return None *)
    | [] -> None
    | rule::rest -> 
        let suffix = match_term func rule acceptor (acc@[startSym, rule]) frag in 
        match suffix with
            | None -> match_nonterm startSym rest func acceptor acc frag
            | _ -> suffix
and match_term func rule acceptor acc frag =
    match rule with
    | [] -> (acceptor frag)
    | (N non_term)::rest -> (match_nonterm non_term (func non_term) func (match_term func rest acceptor acc) acc frag)
    | (T term)::rest -> match frag with 
                                | [] -> None
                                | first::rest_of_frag -> if term = first then match_term func rest acceptor acc rest_of_frag else None

     
let make_matcher gram acceptor frag = 
	match_nonterm (fst gram) ((snd gram) (fst gram)) (snd gram) acceptor [] frag
   
let rec list_subset a b = match a with
	| [] -> true
	| hd::tl -> if List.mem hd b then list_subset tl b else false
    
let rec list_diff a b = match a with
    | [] -> b
    | hda::tla -> match b with 
        | hdb:: tlb -> if hda = hdb then list_diff tla tlb else b
        | [] -> []
        
let rec parse_nonterm tree startSym rules func acceptor acc frag = 
    match rules with
    (* If there are no more rules that can match the nonterminal, then we can't match fragment; return None *)
    | [] -> None
    | hd::tl -> (let temp_tree = parse_term startSym tree func hd acceptor [] frag in 
        match temp_tree with
            | None -> parse_nonterm tree startSym tl func acceptor acc frag
            | Some x -> Some (Node (startSym, x)))
            
        
and parse_term symb tree func rule acceptor acc frag = match rule with
    | [] -> acceptor acc
    | (T term) :: tl -> (match list_diff (parse_tree_leaves (Node(symb, tree))) frag with 
        | [] -> None 
        | first::rest_of_frag -> if term = first then parse_term symb (tree@[Leaf term]) func tl acceptor (acc@[Leaf term]) frag else None
        )
    | (N non_term)::tl -> let temp = parse_nonterm tree non_term (func non_term) func acceptor acc frag in
        match temp with
            | None -> None
            | Some x -> match list_diff (parse_tree_leaves (Node(symb, tree))) frag with 
                | [] -> None 
                | fraghd:: fragtl -> parse_term symb (tree@[x]) func tl acceptor (acc@[x]) frag
    
    
let accept_all = function 
    | x -> Some x
    
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x
   
let make_parser gram frag =  if make_matcher gram accept_empty_suffix frag = Some [] then
    (parse_nonterm [] (fst gram) ((snd gram) (fst gram)) (snd gram) accept_all [] frag)
    else None
(*
type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num
    
let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])
      
make_parser awkish_grammar ["++"; "$"; "1"];;
make_parser awkish_grammar ["$"; "1"; "++"];;
parse_nonterm [] (Expr) ((snd awkish_grammar) Expr ) (snd awkish_grammar) accept_all [] ["$"; "1"; "++"];;

parse_nonterm [] (Term) ((snd awkish_grammar) Term ) (snd awkish_grammar) accept_all [] ["$"; "1"; "++"];;

parse_term Term [] (snd awkish_grammar) [ N Lvalue] accept_all [] ["$"; "1"; "++"];;

parse_term Expr [] (snd awkish_grammar) [N Incrop; N Lvalue] accept_all [] ["++" ; "5"];;
parse_nonterm [] (Incrop) ((snd awkish_grammar) (Incrop)) (snd awkish_grammar) accept_all [] ["++" ;"5"; ];;
parse_term Term [(Node (Incrop, [Leaf "++"]))] (snd awkish_grammar) [N Lvalue] accept_all [(Node (Incrop, [Leaf "++"]))] ["++" ; "5";]
parse_nonterm [(Node (Incrop, [Leaf "++"]))] (Lvalue) ((snd awkish_grammar) (Lvalue)) (snd awkish_grammar) accept_all [] ["++" ;"5"; ];;
parse_term Lvalue [(Node (Incrop, [Leaf "++"]))] (snd awkish_grammar) [T "$"; N Expr] accept_all [(Node (Incrop, [Leaf "++"]))] ["++" ; "5";]
*)