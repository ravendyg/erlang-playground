-module(h03).

% -export([getUsers/0, get_females/1, get_id_name/1, get_female_id_name/1,
% 				get_stat/1
% ]).

-compile(export_all).

getUsers() ->
	[{user, 1, "sdfsdf", male, 22},
	{user, 2, "weqwe", female, 14},
	{user, 3, "opkop", male, 11},
	{user, 4, "weopmnvn", female, 18}].

get_females(Users) ->
	lists:filter(fun({user, _, _, Gender, _}) -> Gender =:= female end, Users).

get_id_name(Users) ->
	lists:map(fun({user, Id, Name, _, _}) -> {Id, Name} end, Users).

% get_female_id_name(Users) ->
% 	Females = lists:filter(fun({user, _, _, Gender, _}) -> Gender =:= female end, Users),
% 	lists:map(fun({user, Id, Name, _, _}) -> {Id, Name} end, Females).

get_female_id_name(Users) ->
	lists:filtermap(
		fun({user, _Id, _Name, male, _}) -> false;
				({user, Id, Name, female, _}) -> {true, {Id, Name}}
	end, Users).

get_stat(Users) ->
	lists:foldl(
		fun({user, _, _, female, Age}, {FCount, MCount, TCount, TotalAge}) ->
					{FCount+1, MCount, TCount+1, TotalAge + Age};
				({user, _, _, male, Age}, {FCount, MCount, TCount, TotalAge}) ->
					{FCount, MCount+1, TCount+1, TotalAge + Age}
		end,
		{0, 0, 0, 0},
		Users
	).

