% some stuff from https://www.coursera.org/learn/algorithms-on-strings
-module(str).

-compile(export_all).

patternInsert([], Node) -> io:format("3~p~n", Node),Node;
% patternInsert([H], []) ->
% 	io:format("3~p~n", H),
% 	[{H, []}];
% patternInsert([H|T], []) ->
% 	io:format("4~p~n", H),
% 	[{H, patternInsert(T, [])}];
patternInsert([H], Node) ->
	io:format("1~p~n", H),
	{Ls, _, Gt} = split(H, Node),
	Ls ++ [{H, []}] ++ Gt;
patternInsert([H|T], Node) ->
	{Ls, Eq, Gt} = split(H, Node),
	io:format("2~p~n", H),
	case Eq of
		[] 					 	 -> Ls ++ [{H, patternInsert(T, [])}] ++ Gt;
		[{Val, NNode}] -> Ls ++ [{Val, patternInsert(T, NNode)}] ++ Gt
	end.

split (_, []) -> {[],[],[]};
split (H, Node) -> split(H, Node, {[],[],[]}).
split (_, [], Acc) -> Acc;
split (H, [{Val, Node}|T], {Vl, [], Vg})
	when H == Val -> split(H, T, {Vl, [{Val, Node}], Vg});
split (H, [{Val, Node}|T], {Vl, Ve, Vg})
	when H > Val  -> split(H, T, {[{Val, Node}|Vl], Ve, Vg});
split (H, [{Val, Node}|T], {Vl, Ve, Vg})
	when H < Val  -> split(H, T, {Vl, Ve, [{Val, Node}|Vg]}).

emptyNode () -> nil.