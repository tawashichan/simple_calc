module Ast = struct
	type definition = Def of string * expr * expr | Expr of expr
	and expr = Num of int | Var of string | App of expr * expr | Pair of expr * expr 
	and nothing = Nothing
end