type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

let convert_grammar gram1= 
    let rec combine_rules grammar nonterminal_symbol = 
        match grammar with
            | [] -> []
            | (nt, curr_rule)::rules -> 
                if nt = nonterminal_symbol
                then curr_rule :: (combine_rules rules nonterminal_symbol)
                else combine_rules rules nonterminal_symbol
    in (fst gram1, (combine_rules (snd gram1)))

let rec parse_tree_leaves tree = 
    match tree with
        | Leaf x -> [x]
        | Node (_, tree_list) -> 
            let rec parse_tree_list tree_list =
                match tree_list with 
                    | [] -> []
                    | head::rest -> (parse_tree_leaves head)@(parse_tree_list rest)
            in parse_tree_list tree_list

let make_matcher gram = fun accept frag ->
    let rec parse_rule_list rule_function rule_list accept frag =
        let rec parse_rule rules accept frag =
            match rules with
                | [] -> accept frag
                | T next_symbol::rest_rules ->
                    (match frag with
                        | [] -> None
                        | frag_symbol::rest_frag -> 
                            if frag_symbol = next_symbol 
                            then parse_rule rest_rules accept rest_frag
                            else None)
                | N next_symbol::rest_rules -> parse_rule_list rule_function (rule_function next_symbol) (parse_rule rest_rules accept) frag      
        in match rule_list with
            | [] -> None
            | curr_rules::rest_rules ->
                match parse_rule curr_rules accept frag with
                    | Some s -> Some s
                    | None -> parse_rule_list rule_function rest_rules accept frag
    in parse_rule_list (snd gram) ((snd gram) (fst gram)) accept frag

let make_parser gram = 
    let rec parse_rule_list_concat rule_function rule_list accept frag =
        let rec parse_rule_concat rules accept frag =
            match rules with
                | [] -> accept frag
                | T next_symbol::rest_rules ->
                    (match frag with
                        | [] -> None
                        | frag_symbol::rest_frag -> 
                            if frag_symbol = next_symbol 
                            then parse_rule_concat rest_rules accept rest_frag
                            else None)
                | N next_symbol::rest_rules -> parse_rule_list_concat rule_function (rule_function next_symbol) (parse_rule_concat rest_rules accept) frag      
        in match rule_list with
            | [] -> None
            | curr_rules::rest_rules ->
                match parse_rule_concat curr_rules accept frag with
                    | Some s -> Some (curr_rules::s)
                    | None -> parse_rule_list_concat rule_function rest_rules accept frag
    in fun frag ->
        match parse_rule_list_concat (snd gram) ((snd gram) (fst gram)) (function | [] -> Some [] | _ -> None) frag with 
            | None -> None
            | Some s ->  
                if s = [] 
                then None
                else 
                    let rec convert_derivation_to_tree derivation = 
                        let rec scan_derivation derivation = function
                            | T terminal_symbol -> (Leaf terminal_symbol, derivation)
                            | N nonterminal_symbol -> 
                                match derivation with
                                    | [] -> (Node (nonterminal_symbol, []), [])
                                    | curr_rule::rest_rules -> 
                                        let tree = (convert_derivation_to_tree rest_rules curr_rule) 
                                        in (Node (nonterminal_symbol, (fst tree)), (snd tree))
                        in function
                            | [] -> ([], derivation)
                            | rest_rules::curr_rule -> 
                                let horizontal_tree = (scan_derivation derivation rest_rules)
                                in let full_tree = (convert_derivation_to_tree (snd horizontal_tree) curr_rule)
                                in ((fst horizontal_tree)::(fst full_tree), (snd full_tree))
                    in match (fst (convert_derivation_to_tree s [N (fst gram)])) with
                        | tree::rest -> Some (tree)
                        | [] -> None
