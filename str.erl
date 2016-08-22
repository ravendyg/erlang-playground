% some stuff from https://www.coursera.org/learn/algorithms-on-strings
-module(str).

-compile(export_all).

patternInsert([], Node)
	when length(Node) == 0 -> [];

patternInsert([], Node) -> Node;

% patternInsert([H|T], []) -> [{H, patternInsert(T, emptyNode())}].

patternInsert([H|T], Node)
	when length(Node) == 0 -> [{H, patternInsert(T, emptyNode())}];

patternInsert([H|T], Node) ->
	Out = lists:filter(fun ({Val, _}) -> Val =/= H end, Node),
	case length(Out) == length(Node) of
		true  -> Out ++ [{H, patternInsert(T, emptyNode())}];
		false ->
			[{_, OldNode}|_] = lists:filter(fun ({Val, _}) -> Val == H end, Node),
			Out ++ [{H, patternInsert(T, OldNode)}]
	end.

emptyNode () -> [].