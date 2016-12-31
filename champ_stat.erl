-module(champ_stat).

-export([get_stat/1]).
-include_lib("eunit/include/eunit.hrl").


% get_stat(Champ) -> get_stat(Champ, {0, 0, 0, 0}).


% get_stat([], {Teams, Players, Age, Rating}) -> {Teams, Players, Age / Players, Rating / Players };

% get_stat([H | T], {Teams, Players, Age, Rating}) ->
%     {Count, PA, PR} = lists:foldl(
%         fun({player, _, Ag, Ra, _}, {Count, TA, TR}) -> {Count+1, TA + Ag, TR + Ra} end,
%         {0, 0, 0},
%         element(3, H)
%     ),
%     get_stat(T, {Teams+1, Players + Count, Age + PA, Rating + PR}).

get_stat(Champ) ->
    {Teams, Players, TotalAge, TotalRating} =
			lists:foldl(fun fold_team_list/2, {0, 0, 0, 0}, Champ),
    {Teams, Players, TotalAge / Players, TotalRating / Players}.

fold_player_list ({player, _, Age, Rating, _}, {Count, TotalAge, TotalRating}) ->
    {Count+1, TotalAge + Age, TotalRating + Rating}.

fold_team_list ({team, _Name, Players}, {TeamCount, PlayerCount, TotalAge, TotalRating}) ->
    {Count, PA, PR} = lists:foldl(fun fold_player_list/2, {0, 0, 0}, Players),
    {TeamCount+1, PlayerCount+Count, TotalAge+PA, TotalRating+PR}.

get_stat_test() ->
    ?assertEqual({5,40,24.85,242.8}, get_stat(champ:sample_champ())),
    ok.
