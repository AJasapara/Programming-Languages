let my_subset_test0 = not(subset [3;2] [])
let my_subset_test1 = subset [2;1;2;1;2] [1;2]
let my_subset_test2 = subset [1;2;3] [5;4;3;2;1]

let my_equal_sets_test0 = equal_sets [2;1;2;1;2] [1;2]
let my_equal_sets_test1 = not (equal_sets [2] [2;3;4])

let my_set_union_test0 = equal_sets (set_union [1;2;3] [1;2;3]) [1;2;3]
let my_set_union_test1 = not (equal_sets (set_union [3;1] [2;3]) [1;2])
let my_set_union_test2 = equal_sets (set_union [1;2;3;4] [1]) [1;2;3;4]

let my_set_intersection_test0 =
  equal_sets (set_intersection [1] [1;2;3]) [1]
let my_set_intersection_test1 =
  equal_sets (set_intersection [1;2;3] [1;2;3]) [1;2;3]
let my_set_intersection_test2 =
  not(equal_sets (set_intersection [1;2;3;4;5;6;7] [8;9;10]) [1])

let my_set_diff_test0 = equal_sets (set_diff [1;2] []) [1;2]
let my_set_diff_test1 = equal_sets (set_diff [4] [1;3]) [4]
let my_set_diff_test2 = not(equal_sets (set_diff [4;2;5] [2;5]) [4;2;5])

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x * 10) 0 = 0
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x * 2 / 4) 1234567890 = 0

type test_nonterminals =
  | Hi | Bye | Hello | Goodbye | Unreachable

let test_rules =
  [Hi, [T "Hi"; N Hello];
   Hi, [N Bye; N Goodbye];
   Bye, [T "Bye"];
   Bye, [N Goodbye];
   Hello, [T "Hello"; N Hi];
   Hello, [N Goodbye];
   Goodbye, [T "Goodbye"];
   Unreachable, [T "I shouldn't exist!"; N Hi];
   Unreachable, [N Bye; N Goodbye]]

let test_grammar = Hi, test_rules

let my_filter_reachable_test0 =
  filter_reachable test_grammar = 
  (Hi, 
    [Hi, [T "Hi"; N Hello];
     Hi, [N Bye; N Goodbye];
     Bye, [T "Bye"];
     Bye, [N Goodbye];
     Hello, [T "Hello"; N Hi];
     Hello, [N Goodbye];
     Goodbye, [T "Goodbye"]])

let my_filter_reachable_test1 =
  filter_reachable (Hi, List.tl test_rules) = 
  (Hi, 
     [Hi, [N Bye; N Goodbye];
     Bye, [T "Bye"];
     Bye, [N Goodbye];
     Goodbye, [T "Goodbye"]])

let my_filter_reachable_test2 =
  filter_reachable (Bye, test_rules) = 
  (Bye, 
     [Bye, [T "Bye"];
     Bye, [N Goodbye];
     Goodbye, [T "Goodbye"]])