(*subset function:
 for every num in a, check if it is in b
 *)
let rec subset a b = match a with
	| [] -> true
	| hd::tl -> if List.mem hd b then subset tl b else false;;
(*equal sets function:
 for every num in a, check if it is in b, if it is, use the function again on a filtered list of a and b such that the num and all repeats are gone from both lists. terminate if at the end both lists are empty
 *)
let rec equal_sets a b = match a,b with
	| [], [] -> true
	| [], notempty -> false
	| hd::tl, temp -> if List.mem hd temp then equal_sets (List.filter (fun x -> not (x=hd)) tl) (List.filter (fun x -> not (x=hd)) temp) else false;;
let set_union a b = a @ b;;
let set_intersection a b = List.filter (fun x -> (List.mem x a)) b;;
let set_diff a b = List.filter (fun x -> not (List.mem x b)) a;;
let rec computed_fixed_point eq f x = if eq (f x) x then x else computed_fixed_point eq f (f x);;

type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal;;

let is_non_terminal rule = 
  match rule with
  | T x -> false
  | N x -> true;;
  
(* to get rid of the N class*)
let strip_terminality rule = match rule with
	| N x -> x ;;
	
(* for a list of rules such as [N Expr; N Binop; T "++"; N Expr] return all non terminals with their N class stripped*)
let rec all_non_terminals accum rule = match rule with
	| [] -> accum
	| hd::tl -> if is_non_terminal hd then all_non_terminals (accum@[strip_terminality hd]) tl else all_non_terminals accum tl;;

(* return all nonterminals found. this will try to refind new expressions if it finds a new one using a duplicate of the original rules*)
let rec filter_helper rules found rulesdup = match rules with
	| [] -> found
	| (expr, rule):: rest -> if (List.mem expr found) && not (subset (all_non_terminals [] rule) found) then filter_helper rulesdup (found@(all_non_terminals [] rule)) rulesdup else filter_helper rest found rulesdup

(*using all nonterminals found from filter_helper, pick correct rules*)
let rec filter_collector accum rules found = match rules with
	| [] -> accum
	| (expr, rule) :: rest -> if (List.mem expr found) then filter_collector (accum@[(expr,rule)]) rest found else filter_collector accum rest found

let filter_reachable g = 
	(fst g), (filter_collector [] (snd g) (filter_helper (snd g) [fst g] (snd g)))
	
