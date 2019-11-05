open Tipo;;

module StackABS(S:Tipo)=
struct 
	type stack = {mutable c:S.t list}
	exception EmptyStack
	
	let push s elm = s.c<-(elm::s.c);s
	
	let top s = 
		match s.c with
			[]->raise EmptyStack
			|h::t->h
	
	let pop s =
		match s.c with
			[]->raise EmptyStack
			|h::t->s.c<-t;h
	
	let empty() = {c=[]}
		
end;;
