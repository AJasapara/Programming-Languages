let accept_all string = Some string
type awkarpit_nonterminals = | Hi | Hello | Bye | Goodbye

let awkarpit_grammar = 
	(Hi, 
		function
		| Hi -> [[N Hello]; [T "Hi"]; [N Bye; N Goodbye]]
		| Hello -> [[N Goodbye]; [T "Hello"]]
		| Bye -> [[N Goodbye];[T "Bye"]]
		| Goodbye -> [[T "Goodbye"]])

let make_matcher_test = ((make_matcher awkarpit_grammar accept_all ["Goodbye";"Goodbye";"Goodbye";"Hi"]) = Some ["Goodbye"; "Goodbye"; "Hi"])

let small_arpit_frag = ["Bye"; "Goodbye"]

let make_parser_intermediate_test = ((make_parser awkarpit_grammar small_arpit_frag) = 
	Some (Node (Hi, [Node (Bye, [Leaf "Bye"]); Node (Goodbye, [Leaf "Goodbye"])])))

let make_parser_test =
  match make_parser awkarpit_grammar small_arpit_frag with
    | Some tree -> parse_tree_leaves tree = small_arpit_frag
    | _ -> false