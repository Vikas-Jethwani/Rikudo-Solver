rikudo(N,Pre,Links,Result):-
		assert(bounds(91,10,5)),
		assert(bounds(61,8,4)),
		assert(bounds(37,6,3)),
		assert(bounds(19,4,2)),
		assert(bounds(7,2,1)),

		%% store_pre(Pre,Visited),
		find_start(N,Pre,X,Y),
		update_links(Links,Links2),
		solve(X,Y,N,1,[(0,0,-1)],Pre,Result,Links2),
		check_links(Links,Result),
		retractall(bounds(_,_,_)).

%% find_start(N,[],-Y,Y):- bounds(N,_,Y).
%% find_start(N,[(X,Y,_,1)|T],X,Y):-!.
%% find_start(N,[(_,_,_,Num)|T],X,Y):-Num=\=1,find_start(N,T,X,Y).

find_start(N,[],Y,-Y):- bounds(N,_,Y),!.
find_start(N,[(X,Y,1)|T],X,Y):-!.
find_start(N,[(_,_,Num)|T],X,Y):-Num=\=1,find_start(N,T,X,Y).


update_links([],[]):-!.
update_links([(X1,Y1,X2,Y2)|T1], [(X1,Y1,X2,Y2),(X2,Y2,X1,Y1)|T2]):-
		update_links(T1,T2).

check_links([],_):-!.
check_links([(X1,Y1,X2,Y2)|T],Result):-
		get_val(X1,Y1,Result,Num1),
		get_val(X2,Y2,Result,Num2),
		Temp is abs(Num1-Num2),
		Temp =:= 1,
		check_links(T,Result).

get_val(X,Y,[(X,Y,Num)|_],Num):-!.
get_val(X,Y,[(_,_,_)|T],Num):-
		get_val(X,Y,T,Num).


store_pre([],[(0,0,'B',-1)]).
store_pre([(X,Y,Num)|T], [(X,Y,'B',Num)|TT]):-store_pre(T,TT).

%% if member 
%% isMember([],X,Y, false, 'B', -2):-!.
%% isMember([(X,Y,C,Num)|T],X,Y, true, C, Num):-!.
%% isMember([(X1,Y1,_,_)|T],X,Y, TF, C, Num):-
%% 		isMember(T,X,Y,TF,C,Num).


isMember([(X,Y,_)|_],X,Y):-!.
isMember([(_,_,_)|T],X,Y):-
		isMember(T,X,Y).

fulfilPre(X,Y,Curr,Curr,[]):-!.
fulfilPre(X,Y,Curr,Curr1,[(X,Y,Curr1)|T]):-!.
fulfilPre(X,Y,Curr,Req,[(X1,Y1,_)|T]):- fulfilPre(X,Y,Curr,Req,T).


%% X, Y, N, Curr, Visited, Pre, Result
solve(X, Y, N, N, Visited, Pre, Visited, Links2).
solve(X, Y, N, Curr, Visited, Pre, Result, Links2):-

	bounds(N, DiagBound, TopBottom),
	(abs(X)+abs(Y) =< DiagBound),
	Y =< TopBottom, Y >= -TopBottom,

	not(isMember(Visited,X,Y)),

	fulfilPre(X, Y, Curr, Req, Pre),

	Curr=:=Req,

	N2 is Curr+1,
	( isLink(X,Y,X2,Y2,Links2),not(isMember(Visited,X2,Y2)) -> (solve(X2,Y2,N,N2,[(X,Y,Curr)|Visited],Pre,Result,Links2)) ; (callnbrs(X,Y,N,N2,[(X,Y,Curr)|Visited],Pre,Result,Links2))).

	
isLink(X1,Y1,X2,Y2,[(X1,Y1,X2,Y2)|_]):-!.
isLink(X1,Y1,X2,Y2,[(_,_,_,_)|T]):-
		isLink(X1,Y1,X2,Y2,T).


callnbrs(X,Y,N,Curr,Visited,Pre,Result,Links2):-
	Xd is X+1, Yd is Y+1, solve(Xd,Yd,N,Curr,Visited,Pre,Result,Links2),!.
callnbrs(X,Y,N,Curr,Visited,Pre,Result,Links2):-
	Xd is X+2, solve(Xd,Y,N,Curr,Visited,Pre,Result,Links2),!.
callnbrs(X,Y,N,Curr,Visited,Pre,Result,Links2):-
	Xd is X+1, Yd is Y-1, solve(Xd,Yd,N,Curr,Visited,Pre,Result,Links2),!.

callnbrs(X,Y,N,Curr,Visited,Pre,Result,Links2):-
	Xd is X-1, Yd is Y+1, solve(Xd,Yd,N,Curr,Visited,Pre,Result,Links2),!.
callnbrs(X,Y,N,Curr,Visited,Pre,Result,Links2):-
	Xd is X-2, solve(Xd,Y,N,Curr,Visited,Pre,Result,Links2),!.
callnbrs(X,Y,N,Curr,Visited,Pre,Result,Links2):-
	Xd is X-1, Yd is Y-1, solve(Xd,Yd,N,Curr,Visited,Pre,Result,Links2),!.


