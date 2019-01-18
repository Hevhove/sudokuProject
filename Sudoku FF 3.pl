% suggested query: main.

:- use_module(library(clpfd)).

main :-
	solutions
	;
	halt.

main2 :-
	solutions
	;
	fail.

solutions :-
	statistics(runtime,_),
	fd_statistics(backtracks,_),
	write('=========================[ Solutions ]========================='),
	nl,
	(	the_problem(__S),
		write('[X] = '), write(__S),
		nl,
		fail
	;
		write('=======================[ End Solutions ]======================='),
		nl,
		fail
	).

the_problem([X]) :-
	length(X,81),
	domain(X,1,9),
	numbered_list(X,XL,0),
	all_constraints([XL]),
	myappend([],X,__Tmp1),
	labeling([ffc, step, up], __Tmp1),
	fd_statistics(backtracks,__BT),
	write('Backtracks: '), write(__BT), nl,
	statistics(runtime,[_,__RT]),
	__RTsec is __RT / 1000,
	write('Runtime: '), write(__RTsec), write('s'), nl.

find_var([(V,N)|_], N, V) :- !.
find_var([_|Vs], N, V) :-
	find_var(Vs, N, V).

numbered_list([X],[(X,Acc)],Acc) :- !.
numbered_list([X|Xs],[(X,Acc)|XNs],Acc) :-
	Acc1 is Acc + 1,
	numbered_list(Xs,XNs,Acc1).

myappend([],L,L).
myappend([X|L1],L2,[X|L12]) :-
	myappend(L1,L2,L12).

all_constraints([X]) :-
	write('Adding constraint "'), write('X_I \\= X_J"'), write(' for values:'), nl,
	the_constraint1(X,X),
	write('Adding constraint "'), write('X_I \\= X_J"'), write(' for values:'), nl,
	the_constraint2(X,X),
	write('Adding constraint "'), write('X_I \\= X_J"'), write(' for values:'), nl,
	the_constraint3(X,X),
sudoku(X).

the_constraint1([],_).
the_constraint1([X_I|X_Is],X_J) :-
	the_constraint1_aux(X_I,X_J),
	the_constraint1(X_Is,X_J).

the_constraint1_aux(_,[]).
the_constraint1_aux(X_I,[X_J|X_Js]) :-
	the_constraint1_aux_aux(X_I,X_J),
	the_constraint1_aux(X_I,X_Js).

the_constraint1_aux_aux((X_I,I), (X_J,J)) :- !,
	(I//9 =:= J//9 , I =\= J ->
		write('\t'),
		write('I='), write(I), write(', '),
		write('J='), write(J), write(', '),
		nl,
		X_I #\= X_J
	;
		true
	).
the_constraint1_aux_aux(_,_).

the_constraint2([],_).
the_constraint2([X_I|X_Is],X_J) :-
	the_constraint2_aux(X_I,X_J),
	the_constraint2(X_Is,X_J).

the_constraint2_aux(_,[]).
the_constraint2_aux(X_I,[X_J|X_Js]) :-
	the_constraint2_aux_aux(X_I,X_J),
	the_constraint2_aux(X_I,X_Js).

the_constraint2_aux_aux((X_I,I), (X_J,J)) :- !,
	(I mod 9 =:= J mod 9 , I =\= J ->
		write('\t'),
		write('I='), write(I), write(', '),
		write('J='), write(J), write(', '),
		nl,
		X_I #\= X_J
	;
		true
	).
the_constraint2_aux_aux(_,_).

the_constraint3([],_).
the_constraint3([X_I|X_Is],X_J) :-
	the_constraint3_aux(X_I,X_J),
	the_constraint3(X_Is,X_J).

the_constraint3_aux(_,[]).
the_constraint3_aux(X_I,[X_J|X_Js]) :-
	the_constraint3_aux_aux(X_I,X_J),
	the_constraint3_aux(X_I,X_Js).

the_constraint3_aux_aux((X_I,I), (X_J,J)) :- !,
	((I mod 9) //3 =:= (J mod 9) //3 , I//27 =:= J//27 , I =\= J ->
		write('\t'),
		write('I='), write(I), write(', '),
		write('J='), write(J), write(', '),
		nl,
		X_I #\= X_J
	;
		true
	).
the_constraint3_aux_aux(_,_).

sudoku([]).
sudoku([X|Xs]) :-
	sudoku_constraint(X),
	sudoku(Xs).

sudoku_constraint((X,1)) :- !, X = 4.
sudoku_constraint((X,2)) :- !, X = 2.
sudoku_constraint((X,4)) :- !, X = 8.
sudoku_constraint((X,7)) :- !, X = 9.
sudoku_constraint((X,8)) :- !, X = 5.
sudoku_constraint((X,11)) :- !, X = 3.
sudoku_constraint((X,15)) :- !, X = 8.
sudoku_constraint((X,17)) :- !, X = 2.
sudoku_constraint((X,18)) :- !, X = 8.
sudoku_constraint((X,23)) :- !, X = 2.
sudoku_constraint((X,24)) :- !, X = 1.
sudoku_constraint((X,28)) :- !, X = 5.
sudoku_constraint((X,29)) :- !, X = 7.
sudoku_constraint((X,30)) :- !, X = 6.
sudoku_constraint((X,32)) :- !, X = 4.
sudoku_constraint((X,33)) :- !, X = 9.
sudoku_constraint((X,34)) :- !, X = 3.
sudoku_constraint((X,39)) :- !, X = 3.
sudoku_constraint((X,41)) :- !, X = 7.
sudoku_constraint((X,46)) :- !, X = 6.
sudoku_constraint((X,47)) :- !, X = 4.
sudoku_constraint((X,48)) :- !, X = 2.
sudoku_constraint((X,50)) :- !, X = 8.
sudoku_constraint((X,51)) :- !, X = 5.
sudoku_constraint((X,52)) :- !, X = 1.
sudoku_constraint((X,56)) :- !, X = 6.
sudoku_constraint((X,57)) :- !, X = 9.
sudoku_constraint((X,62)) :- !, X = 1.
sudoku_constraint((X,63)) :- !, X = 9.
sudoku_constraint((X,65)) :- !, X = 8.
sudoku_constraint((X,69)) :- !, X = 7.
sudoku_constraint((X,72)) :- !, X = 5.
sudoku_constraint((X,73)) :- !, X = 7.
sudoku_constraint((X,76)) :- !, X = 3.
sudoku_constraint((X,78)) :- !, X = 4.
sudoku_constraint((X,79)) :- !, X = 2.
sudoku_constraint((_,_)).
