open NaturalI;;

module Natural =
	struct 
		type natural = Succ of natural | Zero;;
		
		exception NegativeNumber;;
		
		exception DivisionByZero;;
		
		let eval n = 
			let rec eval n acc =
				match n with 
					Zero -> acc
					|Succ(x)-> eval x (acc+1)
			in
			eval n 0;;
		
		let convert n =
			let rec convert n acc =
				match n with
					0->acc
					|_->convert (n-1) (Succ(acc))
			in
			convert n Zero;;
		
		let ( + ) a b =
			if a=Zero then b
			else if b = Zero then a
			else
			let c=(eval a) + (eval b)
			in
			convert c;;
		
		let ( - ) a b =
			if b = Zero then a
			else
				let a' = eval a and
					b' = eval b
				in 
				  if b'>a' then raise NegativeNumber
				  else
				   convert (a'-b');;
		
		let ( * ) a b =
			if a=Zero || b= Zero then Zero
			else
			let c=(eval a) * (eval b)
			in
			convert c;;
		
		let ( / ) a b =
			if b=Zero then raise DivisionByZero
			else
			let c=(eval a) / (eval b)
			in
			convert c;;
			
			
		
	end;;

module N = (Natural:NaturalI)
