
(*
...testing convert_grammar and parse_prefix 
*)
type meow_bark_type = 
	| Expr | Num | Struc | Adj | Animal | Verb

let meow_bark_rules = 
	[
	Expr, [N Num; N Struc];
	Expr, [N Struc];
	Struc, [N Adj; N Animal];
	Adj, [T"happy"];
	Adj, [T"sad"];
	Adj, [T"mad"];
	Animal, [T"dogs"; N Verb];
	Animal, [T"cats"; N Verb];
	Animal, [T"are"; T"with"; T"me"];
	Verb, [T"left"];
	Verb, [T"arrived"; T"and"; N Expr];
	Num, [T"3"];
	Num, [T"6"];
	Num, [T"9"];
	] 

let meow_bark_gram1 = Expr, meow_bark_rules

let meow_bark_gram2 = convert_grammar meow_bark_gram1

let rec contains_Number = function
	| []->false
	| (Num, _)::_ -> true
	| _::rules ->contains_Number rules

let doesnt_accept_Number rules frag = 
	if contains_Number rules
	then None
	else Some (rules, frag)

let test_1 = 
	((parse_prefix meow_bark_gram2 doesnt_accept_Number 
		["happy";"dogs";"arrived";"and";"mad";"cats";"left";"9"])
	= Some
	  (
	    [
		(Expr, [N Struc]);(Struc, [N Adj; N Animal]); (Adj, [T"happy"]); 
		(Animal, [T"dogs"; N Verb]); (Verb, [T"arrived"; T"and"; N Expr]);
		(Expr, [N Struc]); (Struc, [N Adj; N Animal]);(Adj, [T"mad"]);
		(Animal, [T"cats"; N Verb]); (Verb, [T"left"])
	    ] , ["9"]
	  ))


(*test case 2*)
let accept_all derivation string = Some (derivation, string)

type address_type = 
	| Add | Street_Num | Street | Apt | City | State | Zip | Direction | Ext

let address_grammar =
	(Add, 
	    function
		| Add -> [[N Street_Num; N Street; N Apt; N City; N State; N Zip];
			[T"can't tell you"]]
		| Street_Num -> [[T"123"];[ T"456"];[T"7890"];[T"N/A"]]
		| Street -> [[T"Glendon"];[T"1st"; N Direction];[T"2nd"; N Direction]]
		| Apt -> [[T"house"];[T"#101"];[T"#102"];[T"#103"]]
		| City -> [[T"LA"];[T"Westwood"];[T"Woodland Hills"]]
		| State -> [[T"CA"];[T"OR"]]
		| Zip-> [[T"90000"];[T"90001";N Ext];[T"90011"; N Ext]]
		| Direction -> [[T"South"];[T"North"];[T"West"];[T"East"]]
		| Ext -> [[T"0010"];[T"1023"]]
	)

let test_2 = 
	((parse_prefix address_grammar accept_all ["456";"Glendon";"#102";"LA";"CA";"90001";
		"0010";"can't tell you";"jk"])
	= Some
	  (
	    [
		(Add,[N Street_Num; N Street; N Apt; N City; N State; N Zip]);
		(Street_Num, [T"456"]); (Street,[T"Glendon"]);
		(Apt,[T"#102"]); (City,[T"LA"]); (State,[T"CA"]);
		(Zip,[T"90001";N Ext]);(Ext, [T"0010"])
	    ], ["can't tell you";"jk"]
	  ))

(*anything recursive to itself or circular dependent nonterminals don't work*)


