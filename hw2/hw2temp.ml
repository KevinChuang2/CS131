let get_rhs_node tuple = match tuple with
    | Node (_,a) -> a
let get_lhs_node tuple = match tuple with
    | Node (_,a) -> a
    
let rec parse_nonterm startSym rules func acceptor acc frag = 
    match rules with
    (* If there are no more rules that can match the nonterminal, then we can't match fragment; return None *)
    | [] -> None
    | rule::rest -> 
        let suffix = parse_term func rule acceptor acc frag in 
        match suffix with
            | None -> parse_nonterm startSym rest func acceptor acc frag
            | _ -> suffix
and parse_term func rule acceptor acc frag =
    match rule with
    | [] -> acceptor frag
    | (N non_term)::rest -> (parse_nonterm non_term (func non_term) func (parse_term func rest acceptor acc) acc frag)
    | (T term)::rest -> match frag with 
                                | [] -> None
                                | first::rest_of_frag -> if term = first then parse_term func rest acceptor acc rest_of_frag else None
                                
                                
let make_parser gram frag =  if make_matcher gram accept_empty_suffix frag = Some [] then
    (parse_nonterm (fst gram) ((snd gram) (fst gram)) (snd gram) accept_empty_suffix [] frag)
    else None
