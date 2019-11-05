 module Expre =
	struct
		type expr = Sum of expr*expr | Dif of expr*expr | Mul of expr*expr | Div of expr*expr | Val of float;;
		
		exception BadFormat;;
		
		let rec to_string expr =
			match expr with
				Val(x)->string_of_float x
				|Sum(a,b)->String.concat " " ["("; (to_string a); "+"; (to_string b); ")"]
				|Dif(a,b)->String.concat " " ["("; (to_string a); "-"; (to_string b); ")"]
				|Mul(a,b)->String.concat " " ["("; (to_string a); "*"; (to_string b); ")"]
				|Div(a,b)->String.concat " " ["("; (to_string a); "/"; (to_string b); ")"];;
		
		let rec resolve expr =
			match expr with 
				Val(x)->x
				|Sum(a,b)->(resolve a)+.(resolve b)
				|Dif(a,b)->(resolve a)-.(resolve b)
				|Mul(a,b)->(resolve a)*.(resolve b)
				|Div(a,b)->(resolve a)/.(resolve b);;
		
		let to_expr str =
			let rec to_expr n =
				match str.[n] with
				'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'->Val(float_of_string ((String.make 1 str.[n])^"."))
				|'+'->Sum((to_expr (n+1)), (to_expr (n+2)))
				|'-'->Dif((to_expr (n+1)), (to_expr (n+2)))
				|'*'->Mul((to_expr (n+1)), (to_expr (n+2)))
				|'/'->Div((to_expr (n+1)), (to_expr (n+2)))
				|_-> raise BadFormat
			in 
			to_expr 0;;				
			
	end;;
