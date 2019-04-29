
let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x
  
   
type kevsub_nonterminals =
  | Hi | Haha | Lol| Hello | Num

let kev_grammar =
  (Hi,
   function
     | Hi ->
         [[N Num; N Hello; N Haha ];
         [T "HI"];
          ]
     | Hello ->
         [[N Lol; N Hi];
         [T"HELLO"]]
     | Lol -> [[T"LOL"]]
     | Haha -> [[T"HAHA"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])
      
let make_matcher_test = (make_matcher kev_grammar accept_all ["8"; "LOL"; "9"; "HELLO"; "HAHA"; "HAHA"] = Some []);;

let make_parser_test = make_parser kev_grammar ["8"; "LOL"; "9"; "HELLO"; "HAHA"; "HAHA"] = Some (Node (Hi,
   [Node (Num, [Leaf "8"]);
    Node (Hello,
     [Node (Lol, [Leaf "LOL"]);
      Node (Hi,
       [Node (Num, [Leaf "9"]); Node (Hello, [Leaf "HELLO"]);
        Node (Haha, [Leaf "HAHA"])])]);
    Node (Haha, [Leaf "HAHA"])]))
;;
