(*Types defined in homework 1 description*)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;


(*
	...converting grammar from homework1 to of homework2
	...homework2_grammar has type (Start Symbol, <fun>)
	   <fun> maps non-terminal to set of rules
*)
let convert_grammar gram1 =
  let rec converting rules non_term = match rules with
	(*
	...[non_termina, rule] is the element of gram1.
	 	we're evaluating nonterminal of the element to see if it's the same as
		non_term input.
	...if so, concatnate the rule with other rules under the same nonterminal
		obtained recursively.
	...else move on to next nonterminal
	*)
    | h::t -> if (fst h) = non_term 
	then (snd h)::(converting t non_term)
	else converting t non_term
    | _ -> []
    in
  ((fst gram1),converting (snd gram1))

;;(*convert_grammar*)




(*
	...Big Idea: Check the rules under the same non-terminal and evaluate the return 
		value of the leftmost rule. The return value determines whether to
		start the process again from the top or returns whatever acceptor
		returns.

	...check_rule evaluates individual rule and branch out according to terminal type. 
*)
let rec match_nt start_sym func sym_rules accept deriv frag =
   match sym_rules with
    | [] -> None
	(*
	...when the set of rules is empty return None
	*)
    | hd_rule::rest_rules ->
        (*
	...check what the leftmost rule returns with the new derivation
	...if None evaluate the remaining rules
	...else return whatever the acceptor returned
	*)
        let new_deriv = deriv@[start_sym, hd_rule] in
	match (check_rules start_sym func hd_rule accept new_deriv frag) with
	  | None -> match_nt start_sym func rest_rules accept deriv frag
	  | Some (rules, frag) -> Some (rules,frag)

  and check_rules start_sym func sym_rules accept der fragment = match sym_rules with
    | [] -> accept der fragment
	(*
	...this is for the last iteration. return whatever acceptor returns
	*)
    | (N nt)::rest_rules_nt -> 
	(*
	...if the first element in rule is nonterminal branch out again as if
		it is the start symbol
	...we still need to evaluate the remaining elements in the rule
	*)
	let new_accept = check_rules start_sym func rest_rules_nt accept in
	match_nt nt func (func nt) new_accept der fragment
    | (T tm)::rest_rule -> match fragment with
	(*
	...when terminal symbol is found, match it with the first element of frag and
		perform tasks on remaining elements in the rule
	...if frag is empty, we cannot do anything but to return None	
	*)
	| []-> None
	| hd_frag::tl_frag -> if (hd_frag = tm)
	  then check_rules start_sym func rest_rule accept der tl_frag (*check next*)
	  else None (*prefix not matching*)

;;(*match_nt*)



let parse_prefix gram accept frag=
	(*
	...returning function(matcher) that accepts acceptor and frag 
	...inputs Start symbol, grammar function, first set of rules, and empty derivation
	*)
  match_nt (fst gram) (snd gram) ((snd gram)(fst gram)) accept [] frag
;;(*parse_prefix*)
