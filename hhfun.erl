-module(hhfun).

-compile(export_all).
%-export([one/0, two/0, add/2]).

-define(sub(X,Y), X-Y).

one () -> 1.

two () -> 2.

ssub(X,Y) -> ?sub(X,Y).

add (X, Y) -> X() + Y().

increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].


map(_,[]) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

reduce(_,[],Acc) -> Acc;
reduce(F,[H|T],Acc) -> reduce(F,T,F(H,Acc)).

incr(X) -> X + 1.
decr(X) -> X - 1.

decr(X,D) -> X - D.

% base(A) ->
%     B = A + 1,
%     F = fun() -> C = A * B end,
%     F(),
%     C=1.

filter(Pred, L) -> lists:reverse( filter(Pred, L, []) ).

filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
  case Pred(H) of
    true  -> filter(Pred, T, [H|Acc]);
    false -> filter(Pred, T, Acc)
  end.

%max([H|T]) -> max2(T,H).

clear () -> io:format("\e[H\e[J").