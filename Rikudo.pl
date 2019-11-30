rikudo(N,Pre,Links,Result):-
		assert(bounds(91,10,5)),
		assert(bounds(61,8,4)),
		assert(bounds(37,6,3)),
		assert(bounds(19,4,2)),
		assert(bounds(7,2,1)),

		assert(n(N)),
		assert(ok(false)),

		assert(point(0,0,'B',-1)),
		store_points(Pre),
		store_links(Links),

		point(X,Y,'B',1),

		N1 is N-1,
		generate(1,N1,All_nums),

		solve(X,Y,N,All_nums),
		output(Result).

output(Result):-retrieve(Result).

retrieve(L):-
	(point(X,Y,C,Num) -> (retractall(point(X,Y,_,_)), retrieve(L1), L = [(X,Y,Num)|L1]) ; L = []).


store_points([]).
store_points([(X,Y,Num)|T]):-
		assert(point(X,Y,'B',Num)),
		store_points(T).

store_links([]).
store_links([(X1,Y1,X2,Y2)|T]):-
		assert(link(X1,Y1,X2,Y2,'N')),
		assert(link(X2,Y2,X1,Y1,'N')),
		store_links(T).

app(X, L, [X|L]).
generate(N2,N2,[N2|[]]).
generate(N1,N2,L):-N1<N2,Newnum1 is (N1+1),generate(Newnum1,N2,L1),app(N1,L1,L).


%% Check cuts in this function
solve(_, _, _, []):-
	(ok(X),X==true -> fail;true),
	retractall(ok(_)),
	assert(ok(true)),
	!.

solve(X, Y, N, [Num|All_nums]):-
	%% write(All_nums), write(X), nl, write(Y), nl, nl,
	Xabs is abs(X),
	Yabs is abs(Y),
	Sum is (Xabs+Yabs),
	
	bounds(N, DiagBound, TopBottom),

	%% Redundant checks.
	(Sum > DiagBound -> fail ; true),
	(Y > TopBottom -> fail ; true),
	(Y < -TopBottom -> fail ; true),
	(X=:=0, Y=:=0 -> fail ; true),
	
	(point(X,Y,C,Given_Num) -> true ; (C = 'W', Given_Num is (-2))),
	(Num =\= Given_Num, C=='B' -> fail ; true ),
	(C=='G' -> fail ; true ),
	%% (Num=:=Given_Num, C=='G' -> fail; true ),

	(C=='W' -> (
				retractall(point(X,Y,_,_)),
				C1 = 'G',
				assert(point(X,Y,C1,Num))
				) 
				; true ),

	(
		listing,
		link(X,Y,Xv,Yv,'N') ->
				(
					retractall(link(X,Y,Xv,Yv,'N')),
					retractall(link(Xv,Yv,X,Y,'N')),
					assert(link(X,Y,Xv,Yv,'V')),
					assert(link(Xv,Yv,X,Y,'V')),
					write(X), nl, write(Y), nl, write(" "), nl, write(Xv), nl, write(Yv), nl, nl,
					solve(Xv,Yv,N,All_nums),
					retractall(link(X,Y,Xv,Yv,'V')),
					retractall(link(Xv,Yv,X,Y,'V')),
					assert(link(X,Y,Xv,Yv,'N')),
					assert(link(Xv,Yv,X,Y,'N')),
					write(X), nl, write(Y), nl, write(" "), nl, write(Xv), nl, write(Yv), nl, nl
				)
				;
				(
					(Xd is X-2, Temp2 is abs(Xd)+Yabs, Temp2 =< DiagBound, solve(Xd,Y,N,All_nums));
					(Xd is X+2, Temp1 is abs(Xd)+Yabs, Temp1 =< DiagBound, solve(Xd,Y,N,All_nums));
					(Xd is X-1, Yd is Y+1, Yd =< TopBottom, solve(Xd,Yd,N,All_nums));
					(Xd is X+1, Yd is Y+1, Yd =< TopBottom, solve(Xd,Yd,N,All_nums));
					(Xd is X-1, Yd is Y-1, Yd >= -TopBottom, solve(Xd,Yd,N,All_nums));
					(Xd is X+1, Yd is Y-1, Yd >= -TopBottom, solve(Xd,Yd,N,All_nums));

					(
						C1=='G' -> (
										retractall(point(X,Y,_,_)),
										assert(point(X,Y,'W',Num)),
										fail
									) 
									;
									fail
					)
				)

	),

	(C1=='G' -> (
				retractall(point(X,Y,_,_)),
				assert(point(X,Y,'W',Num))
				) 
				; true ).
	%% Put cut here.


%% Sample TestCases
%% store_input(7,[(2,0,1)],[]).
%% rikudo(7,[(2,0,1)],[],R).
%% rikudo(37,[(2,0,1)],[],R).
%% rikudo(19,[(2,0,1)],[(-1,-1,1,-1)],R).
%% rik(19,[(2,0,1)],[(-1,-1,1,-1)],R).
%% store_input(7,[(2,0,1),(1,-1,2)],[]).

%% store_input(37,[(4,0,12),(3,-1,1),(1,1,3),(2,2,5),(3,3,8),(-4,0,24),(-6,0,30),(-1,1,36),(1,-3,17),(-3,-3,27)],[]).
%% rikudo(37,[(4,0,12),(3,-1,1),(1,1,3),(2,2,5),(3,3,8),(-4,0,24),(-6,0,30),(-1,1,36),(1,-3,17),(-3,-3,27)],[],R).