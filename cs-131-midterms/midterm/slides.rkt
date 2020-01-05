#lang slideshow

(require slideshow/text)

(define (oc-code text)
  (with-scale 0.7
  (with-font `(bold . modern)
    (para #:align 'left text))))

(define (qa q a)
 (list (list (item q))
       (list (item q a))))

(slide
   #:name "Title"
   (titlet "CS131 Dis1-B-4 Midterm")
   (blank)
   (colorize (t "Xinyu Ma") "blue"))

(slide
 #:title "Midterm"
 (item "Time: 14:00-15:40 Oct 30")
 (subitem "100 minutes")
 (item "Location: Boelter 3400")
 (subitem "The same as lecture")
 (item "Open book & note, but NO electronics")
 (item "Questions are open, answer it" (tt "reasonably"))
 (subitem "There will be one question the professor doesn't know the exact answer."))

(slide
 #:title "1a"
 (para "Write an OCaml function merge_sorted that merges two sorted lists."
       "Its 1st argument should be a comparison function 'lt' that compares two list elements and"
       "returns true if the 1st element is less than the 2nd."
       "Its 2nd and 3rd elements should be the lists to be merged.")
 'next
 (para "merge_sorted (<) [21; 49; 49; 61] [-5; 20; 25; 49; 50; 100] = [-5; 20; 21; 25; 49; 49; 49; 50; 61; 100]"))

(slide
 #:title "1a"
 (oc-code "let rec merge_sorted lt lhs rhs =")
 'next
 (oc-code "match lhs, rhs with")
 'next
 (oc-code "| lh::ltail, rh::rtail ->")
 'next
 (oc-code "  if lt lh rh")
 (oc-code "  then lh::(merge_sorted lt ltail rhs)")
 (oc-code "  else rh::(merge_sorted lt lhs rtail)")
 'next
 (oc-code "| [], _ -> rhs")
 (oc-code "| _ -> lhs"))

(slide
 #:title "1b"
 (para "What is the type of" (tt "merge_sorted") "?")
 'next
 'alts
 (qa "Type of lhs?" (tt "'a list"))
 'next
 (item "Type of rhs?" (tt "'a list"))
 'next
 'alts
 (qa "Type of lt?" (tt "'a -> 'a -> bool"))
 (blank)
 'next
 (para (tt "('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list")))

(slide
 #:title "1c"
 (para "What does the following expression yield, and what is its type?")
 'next
 (with-scale 0.7 (tt "merge_sorted (fun a b -> List.length a < List.length b)"))
 'next
 (para "A function that takes two lists of lists sorted by length, and merge them.")
 (tt "'a list list -> 'a list list -> 'a list list"))

(slide
 #:title "1d"
 (para "Is your implementation of" (tt "merge_sort") "tail-recursive?")
 (item "If so, why it won't have any problem with stack overflow")
 (item "If not, why not, and explain any problems you would have in making it tail-recursive.")
 'next
 (para "The sample code is not. But is it possible?"))

(slide
 #:title "1d"
 (oc-code "let merge_sorted lt =")
 (oc-code "  let rec aux_func acc lhs rhs =")
 (oc-code "    match lhs, rhs with")
 (oc-code "    | lh::ltail, rh::rtail ->")
 (oc-code "      if lt lh rh")
 (oc-code "      then aux_func (lh::acc) ltail rhs")
 (oc-code "      else aux_func (rh::acc) lhs rtail")
 (oc-code "    | [], _ -> List.rev_append acc rhs")
 (oc-code "    | _ -> List.rev_append acc lhs in")
 (oc-code "  aux_func []"))

(slide
 #:title "2"
 (para "Consider the following top-level OCaml definitions")
 (item (tt "let f f = f 1 1"))
 (item (tt "let g g = g 0.0 g"))
 (item (tt "let h h = h f \"x\""))
 (para "For each identifier, give its scope and type."))

(slide
 #:title "2"
 (item (tt "let f f = f 1 1"))
 'next
 (subitem (tt "(inner) f : int -> int -> 'a"))
 (subitem (tt "f : (int -> int -> 'a) -> 'a"))
 'next
 (item (tt "let g g = g 0.0 g"))
 'next
 (subitem (tt "(inner) g : float -> typeof(g) -> 'a"))
 (subitem "Recursive -> error")
 'next
 (item (tt "let h h = h f \"x\""))
 'next
 (subitem (tt "(inner) h : typeof(f) -> string -> 'a"))
 (subitem (small (tt "h : (((int -> int -> 'a) -> 'a) -> string -> 'b) -> 'b"))))

(slide
 #:title "3"
 (item "In Java, is the subtype relation transitive?")
 'next
 (subitem "Yes." (tt "B b = (B)a; C c = (C)b;") "=>" (tt "C c = (C)a;"))
 'next
 (item "In Java, is the graph of the subtype relation a tree? If so, explain why and where is the root;"
       "if not, give a counter example")
 'next
 (subitem "No." (small (tt "class User implements Printable, Comparable<User>"))))

(slide
 #:title "5"
 (para "Suppose we write Java code in a purely functional style,"
       "in that we never assign to any variables except initializing them.")
 (para "In our purely-functional Java programs, is Java Memory Model stll relevant, or we can ignore it?"))

(slide
 #:title "5"
 (para "We can ignore it. Read-only means no conflict.")
 'next
 (para "But we can also answer NO, because the built-in packages are not pure-functional, such as List.")
 (para (tt "list.add(0);") "is not an assignment!"))

(slide
 #:title "4"
 (scale (bitmap "pics/4.png") 0.6 0.7))

(slide
 #:title "4"
 (item "4a. What makes this grammar EBNF and not simply BNF?")
 'next
 (subitem "X?")
 'next
 (item "4b. Give an example that is syntactically correct but semantically incorrect for C.")
 'next
 (subitem (tt "_Noreturn void x=3;")))

(slide
 #:title "4"
 (para "4c. Change the grammar by replacing the ruleset for" (tt "type-qualifier-list") "with")
 (oc-code "type-qualifier-list:")
 (oc-code "  type-qualifier type-qualifier-list?")
 (para "Would this cause any problems?")
 'next
 (item "No."))

(slide
 #:title "4"
 (para "4d. Change the grammar by replacing the ruleset for" (tt "declarator") "with")
 (oc-code "declarator:")
 (oc-code "  pointer? declarator")
 (oc-code "  ID")
 (oc-code "  '(' declarator ')'")
 (oc-code "  declarator '[' INT ']'")
 (oc-code "  declarator '(' void ')'")
 (para "Would this cause any problems?")
 'next
 (item "The language is the same, but the grammar is ambigious.")
 'next
 (para "4e. Draw a syntax chart for the original grammar")
 (para "See 4e.xhtml"))

(slide
 #:title "6"
 (scale (bitmap "pics/6.png") 0.6 0.6))