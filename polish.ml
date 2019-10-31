#use "stackADT.mli";;
#use "stackPol.ml";;

module Polish(S:StackADT)=
  struct
    type expr = N of int
                |Sum of expr*expr
                |Dif of expr*expr
                |Mul of expr*expr
                |Div of expr*expr
								|Pow of expr*expr

		exception BadExpression

		let is_number x =
		 	try	int_of_string x; true
			with e->false;;

		let expr_of_str s=
			let stack = S.empty() in
			let expressions = String.split_on_char ' ' s in
			try
				List.iter (fun x ->
										match x with
											|_ when (is_number x) -> S.push stack (N(int_of_string x))
											|op->match op with
														op when op="+" ->  S.push stack (Sum (S.pop stack , S.pop stack))
														|op when op="-" -> S.push stack (Dif((S.pop stack),(S.pop stack)))
														|op when op="*" -> S.push stack (Mul((S.pop stack),(S.pop stack)))
														|op when op="/" -> S.push stack (Div((S.pop stack),(S.pop stack)))
														|op when op="**" -> S.push stack (Pow((S.pop stack),(S.pop stack)))
														|_->raise BadExpression
									) expressions;
				S.pop stack
			with
			| _ -> raise BadExpression;;

		let rec eval expr =
			match expr with
				N x -> x
				|Sum(a,b)-> (eval a)+(eval b)
				|Dif(a,b)-> (eval a)-(eval b)
				|Mul(a,b)-> (eval a)*(eval b)
				|Div(a,b)-> (eval a)/(eval b)
				|Pow(a,b)-> int_of_float((float_of_int (eval a))**(float_of_int (eval b)))
				|_->raise BadExpression

  end;;
