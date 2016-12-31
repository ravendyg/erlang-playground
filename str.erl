% some stuff from https://www.coursera.org/learn/algorithms-on-strings
-module(str).

-compile(export_all).

patternInsert([], Node)
	when length(Node) == 0 -> [];

patternInsert([], Node) -> Node;

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


match ([], _) -> [];

match (H, Pat) -> matchAll(H, Pat, 1, []).

% iterate over all elements of initial lists
% if match start detected exaust it
% and proceed when it finished
% no matter whether it was positive or negative
matchAll ([], _, _, Acc) -> lists:reverse(Acc);

matchAll (L, Pat, Pos, Acc) ->
	Match = matchOne(L, Pat, Pos, []),
	case Match of
		% no match starting from this point
		[]					->
			matchAll(lists:nthtail(1, L), Pat, Pos + 1, Acc);
		[{In, Elem}]	->
			Len = length(Elem),
			matchAll(lists:nthtail(Len, L), Pat, Pos + Len, [{In, Elem} | Acc])
	end.

% exaust potential match
% until either list (negative) or pattern (positive) ends
matchOne ([], Pat, Pos, Acc) ->
	case length(Pat) == 0 of
		true  -> [{Pos, lists:reverse(Acc)}];
		false -> []
	end;

matchOne ([H|T], Pat, Pos, Acc) ->
	MatchingNode = lists:filter(fun ({Val, _}) -> Val == H end, Pat),
	case length(MatchingNode) > 0 of
		false ->
			[];
		true  ->
			[{N, NextLevel}] = MatchingNode,
			case length(NextLevel) > 0 of
				false ->
					[{Pos, lists:reverse([N|Acc])}];
				true  ->
					matchOne(T, NextLevel, Pos, [H|Acc])
			end
	end.

testPat () ->
	patternInsert([1,2,5,3],
		patternInsert([1,2,4,3],
			patternInsert([1,4,2,3], emptyNode())
		)
	).

emptyNode () -> [].

% str:match([1,1,1,1], str:testPat()). -> []
% str:match([1,2,4,3,1,2,4,3], str:testPat()). -> [{1,[1,2,4,3]},{5,[1,2,4,3]}]
