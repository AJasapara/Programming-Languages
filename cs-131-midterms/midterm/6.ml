let match_empty accept = accept

let match_nothing accept frag = None

let rec match_star matcher accept frag =
  match accept frag with
  | None ->
    matcher (fun frag1 ->
      if frag == frag1
      then None
      else match_star matcher accept frag1) frag
  | ok -> ok

let match_nucleotide nt accept = function
  | [] -> None
  | n::tail -> if n == nt then accept tail else None

let append_matchers matcher1 matcher2 accept =
  matcher1 (matcher2 accept)

let make_append_matchers make_a_matcher ls =
  let rec mams = function
    | [] -> match_empty
    | head::tail -> append_matchers (make_a_matcher head) (mams tail)
  in mams ls
