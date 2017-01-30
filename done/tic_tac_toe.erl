-module(tic_tac_toe).

-export([new_game/0, win/1, move/3]).


new_game() ->
    {{f, f, f},
     {f, f, f},
     {f, f, f}}.


% brute force )
win({{C1,C2,C3},{C4,C5,C6},{C7,C8,C9}}) ->
    if
        C1 =/= f, C1 == C2, C2 == C3 -> {win, C1};
        C4 =/= f, C4 == C5, C5 == C6 -> {win, C4};
        C7 =/= f, C7 == C8, C8 == C9 -> {win, C7};

        C1 =/= f, C1 == C4, C4 == C7 -> {win, C1};
        C2 =/= f, C2 == C5, C5 == C8 -> {win, C2};
        C3 =/= f, C3 == C6, C6 == C9 -> {win, C3};

        C1 =/= f, C1 == C5, C5 == C9 -> {win, C1};
        C3 =/= f, C3 == C5, C5 == C7 -> {win, C3};
        true -> no_win
    end.





move(Cell, Player, GameState) when Cell > 0, Cell < 10 ->
    Row = (Cell - 1) div 3 + 1,
    Col = Cell - (Row - 1) * 3,
    case element(Col, element(Row, GameState)) of
		f -> {ok, setelement(Row, GameState, setelement(Col, element(Row, GameState), Player))};
		_ -> {error, invalid_move}
	end;
move(_, _, _) -> {error, invalid_move}.
