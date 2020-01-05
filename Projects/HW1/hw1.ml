type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec subset a b = 
	match a with 
	[] -> true
	| head :: rest -> if List.mem head b then subset rest b else false;;

let equal_sets a b = subset a b && subset b a;;

let set_diff a b = List.filter (fun a_element -> not (List.mem a_element b)) a;;

let set_union a b = a @ (set_diff b a);;

let set_intersection a b = List.filter (fun a_element -> List.mem a_element b) a;;

let rec computed_fixed_point eq f x = 
	if eq x (f x) then x else computed_fixed_point eq f (f x);;

let rec get_nonterminals symbol_list = 
	match symbol_list with
		| [] -> []
		| T _ :: rest -> get_nonterminals rest
		| N head :: rest -> [head] @ get_nonterminals rest;;

let rec get_other_reachable_symbols nonterminal_symbol grammar = 
	[nonterminal_symbol] @ (match grammar with
		| [] -> []
		| head::rest -> (get_other_reachable_symbols nonterminal_symbol rest) @ 
			if (fst head) = nonterminal_symbol 
			then (get_nonterminals (snd head))
			else []);;

let rec reachable_nonterminals nonterminal_list grammar =
	match nonterminal_list with
		| [] -> []
		| head::rest -> (get_other_reachable_symbols head grammar) @ (reachable_nonterminals rest grammar);;

let rec filter_reachable g = 
	let start = (fst g) in
	let grammar = (snd g) in
	start, (List.filter 
		(fun (rule) -> 
			List.mem (fst rule)  
			(computed_fixed_point 
				equal_sets 
				(fun nonterminal_list -> (reachable_nonterminals nonterminal_list grammar)) 
				[start]))
		grammar);;