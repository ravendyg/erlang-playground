-module(us).

-define(MACR, 2).

-export([add/2, hello/0, both/1, greet/2, head/1, comp/1, wrong_age/1, ttest/1, insert/2, ccase/1, ccasel/1, fac/1, len/1, duplicate/2, sublist/2, reverse/1, zip/2,part/2,qs/1]).

comp (X) when X>=16 ->
    true;
comp (_) -> false.

insert(X,[]) ->
    [X];
insert(X,Set) ->
    case lists:member(X,Set) of
	true  ->  Set;
	false -> [X|Set]
    end.


head(L) -> L.


greet(male,   Name) -> io:format("Hello, Mr. ~s~n",  [Name]);
greet(female, Name) -> io:format("Hello, Mrs. ~s~n", [Name]);
greet(_,      Name) -> io:format("Hello, ~s~n",      [Name]).

wrong_age(X) when X < 16; X > 106 ->
    true;
wrong_age(_) ->
    false.

ttest(X) ->
    if X =:= 10 ->
	 works;
    true -> always
end.
%if X =:= 1 ->
%	works
%    end,
%    if X=:=2;
%       X=:=1 ->
%	works
%    end,
%    if X=:=20 ->
%	fails
%    end,
%    true ->
%	always
%end.

ccase(X) ->
    case X of
	{celsius, N} when N >= 20,
			  N =< 45 ->
	    'favorable';
	{kelvin, N}  when N >= 293,
			  N =< 318 ->
	    'favorable';
	_ ->
	    'avoid beach'
end.

ccasel( {S, N} ) when S =:= kelvin,
		      N >= 293,
		      N =< 318 ->
    'favorable';
ccasel( _ ) -> 'avoid beach'.


add(A,B) ->
    Q = A + B,
    Q + B + ?MACR.

hello() ->
    io:format("Hello, World!, from " ++ ?FILE ++ ".~n").

both(X) ->
    hello(),
    add(X,2).

fac(0) -> 1;
fac(N) when N > 0  -> N * fac(N - 1).

lenPr([], Acc) -> Acc;
lenPr([_|Rest], Acc) -> lenPr(Rest, Acc+1).

len(L) -> lenPr(L, 0).

duplicatePr(0, _, L) -> L;
duplicatePr(C, I, L) -> duplicatePr(C - 1, I, [I | L]).

duplicate(C, I) -> duplicatePr(C, I, []).


subPr(_, Acc, 0) -> Acc;
subPr([], Acc, _)       -> Acc;
subPr([H|T], Acc, I) -> subPr(T, [H|Acc], I - 1).

sublist(L,I) -> reverse( subPr(L,[],I) ).

%reversePr([],Acc)     -> Acc;
%reversePr([H|T], Acc) -> reversePr(T, [H|Acc]).

reverse(L) -> lists:reverse(L).

zipPr([], _, Acc) -> Acc;
zipPr(_, [], Acc) -> Acc;
zipPr([H1|T1], [H2|T2], Acc) -> zipPr(T1, T2, [{H1, H2} | Acc]).

%zip(L1, L2) -> reverse( zipPr(L1, L2, []) ).
zip([],_)  -> [];
zip(_, []) -> [];
zip([X|Xs], [Y|Ys]) -> [ {X,Y} | zip(Xs,Ys) ].

qs([])     -> [];
qs([H|T])  ->
    {Ls, Eq, Gt} = part(H,T),
    qs(Ls) ++ [H|Eq] ++ qs(Gt).

compPr(_,[],R)                         -> R;
compPr(A,[H|L],{Ls,Eq,Gt}) when A > H  -> compPr(A, L, {[H|Ls], Eq, Gt});
compPr(A,[H|L],{Ls,Eq,Gt}) when A < H  -> compPr(A, L, {Ls, Eq, [H|Gt]});
compPr(A,[H|L],{Ls,Eq,Gt}) when A == H -> compPr(A, L, {Ls, [H|Eq], Gt}).

part(A,L) -> compPr(A,lists:reverse(L),{[],[],[]}).


%[{Ls,Eq,Gt} || X <- L, X < A].


-compile([debug_info]).
