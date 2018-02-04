
module Lexer = struct 

	type 'a lex_result = Success of 'a | Failure

	exception Invalid_num
	exception Invalid_symbol

	type token = Num of int | Real of float | ADD | MINUS | MUL | SUB | Lpar | Rpar | Empty | IF | ID
  
	let is_digit str = List.mem str ["0";"1";"2";"3";"4";"5";"6";"7";"8";"9"]
	let is_symbol str = List.mem str ["+";"-";"*";"/";"(";")";"i";"f"]
	let is_par str = List.mem str ["(";")"] 

	let is_empty str = (str = " ") 
	let is_dot str = (str = ".") 

	let get_symbol lst = 
		let rec sub ls str = 
			match ls with 
			[] -> ([],str)
			| first :: rest -> 
			if is_par first then (rest,first)
			else if is_symbol first then sub rest (str ^ first)								
			else if is_empty first then (first :: rest,str)
			else (first :: rest,str)
		in sub lst ""

	let get_symbol_token lst =
		let (rest,str) = get_symbol lst in 
		match str with 
			"+" -> (ADD,rest)
			| "-" -> (MINUS,rest)
			| "*" -> (MUL,rest)
			| "/" -> (SUB,rest)
			| "(" -> (Lpar,rest)
			| ")" -> (Rpar,rest) 
			| "if" -> (IF,rest)	
			| _ -> (Empty,rest)

	let get_num lst = 
		let rec sub ls is_float str = 
			match ls with 
			[] -> ([],is_float,str)
			| first :: rest -> if is_digit first then sub rest is_float (str ^ first)
							   else if is_dot first then sub rest true (str ^ first)	
							   else if is_empty first then (first :: rest,is_float,str)
						 	   else (first :: rest,is_float,str)
		in sub lst false ""
		
	let get_num_token lst = 
		let (rest,is_float,str) = get_num lst in
		if is_float then (Real(float_of_string str),rest)
		else (Num(int_of_string str),rest) 

	(*let get_str_block lst = 
		let rec sub ls result =	match ls with
			[] -> (result,[])
			| first :: rest -> 
				if is_empty first then (result,rest) 
				else if is_digit first then sub rest (result ^ first)
				else if is_symbol first then sub rest (result ^ first) 
				else if is_dot first then sub rest (result ^ first)
				else (result,rest)
		in sub lst "" *)

	let get_str_token lst = 
		let rec sub ls = match ls with
			[] -> (Empty,[])
			| first :: rest -> 
				if is_empty first then (Empty,rest) 
				else if is_digit first || is_dot first then get_num_token ls 
				else if is_symbol first then get_symbol_token ls
				else (Empty,rest)
		in sub lst

	let tokens_to_list lst = 
		let rec sub ls result = 
			match ls with
			[] -> result
			| first :: rest -> 
			let (token,rest_sub) = get_str_token ls in 
			sub rest_sub (token :: result)
		in List.rev (sub lst [])  		

	let str_to_list str = 
		let len = String.length str in
		let rec sub i result = 
			if i < len then sub (i + 1) ((String.sub str i 1) :: result)
			else result 
		in
		List.rev (sub 0 [])

	(*let blocks_to_list lst = 
		let rec sub ls result = 
			match ls with
			[] -> result
			| first :: rest -> 
			let (block,rest_sub) = get_str_block ls in 
			sub rest_sub (block :: result)
		in List.rev (sub lst [])  	

	let str_to_blocks str =
		let lst = str_to_list str in 
		blocks_to_list lst*)

	let str_to_symbols str = 
		let lst = str_to_list str in 
		tokens_to_list lst 

	let rm_empty lst = 
		List.filter (fun x -> x <> Empty) lst

	let run () = 
		let str = read_line() in 
		rm_empty (str_to_symbols str)

end



