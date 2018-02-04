module Lex = Lexer
module As = Ast

module Parser = Parser


let rec examine ast = match ast with
					As.Expr(ex) -> exp ex 
and exp ex = match ex with
					As.Num(num) -> num
				| As.App(As.Var(va),As.Pair (ex1,ex2)) -> let e1 = exp ex1 in 
											let e2 = exp ex2 in
											evaluate va e1 e2

and evaluate va ex1 ex2 = match va with
							"+" -> ex1 + ex2
							| "-" -> ex1 - ex2 
							| "*" -> ex1 * ex2
							| "/" -> ex1 / ex2


let exec () = 
			let tokens = Lex.run() in 
			let ast = Parser.parse tokens in 
			examine ast;;
exec();;			   






