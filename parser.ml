module Lex = Lexer
module As = Ast

module Parser = struct

	exception Syntax_error
	exception Empty_error
	let error () = raise Syntax_error 
	let emp_error () = raise Empty_error


	let rec parse lst = p lst
	and p lst = match lst with
					first :: rest -> (match first with
											_ -> let (_e,rest) = e lst in As.Expr _e	
									)						
					| _ -> error()
	and e lst = let (_t,rest) = t lst in (e' _t rest)
	and e' _e lst = match lst with
					first :: rest -> (match first with
										Lex.ADD -> 	
													let (tok,r) = t rest in 
													let _pre = As.App(As.Var "+",As.Pair(_e,tok)) 
											    	in (e' _pre r)
										| Lex.MINUS -> 	
													let (tok,r) = t rest in 
													let _pre = As.App(As.Var "-",As.Pair(_e,tok)) 
													in (e' _pre r)
										| _ -> (_e,lst)
									)	
					| _ -> (_e,lst)
	and t lst = let (_f,rest) = f lst in (t' _f rest)		
	and t' _t lst = match lst with
					 first :: rest -> (match first with
										Lex.MUL -> 	
													let (tok,r) = f rest in  
													let _pre = As.App(As.Var "*",As.Pair(_t,tok)) 
													in (t' _pre r)
										| Lex.SUB ->
												 	let (tok,r) = f rest in 
													let _pre = As.App(As.Var "/",As.Pair(_t,tok)) 
													in (t' _pre r)
										| _ -> (_t,lst)
									)	
					| _ -> (_t,lst)
	and f lst = match lst with
					first :: rest -> (match first with
									 		 Lex.Num(num) -> (As.Num(num),rest)
											| _ -> error()
									)						
					| _ -> error()		
	

end


