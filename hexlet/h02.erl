-module(h02).

-export([getUsers/0, getFemales/1, splitByAge/1]).

% -compile(export_all).

getUsers() ->
	[{user, 1, "sdfsdf", male, 22},
	{user, 2, "weqwe", female, 14},
	{user, 3, "opkop", male, 11},
	{user, 4, "weopmnvn", female, 18}].


getFemales(Users) -> getFemales(Users, []).

getFemales([User | Rest], Acc) ->
	case User of
		{user, _, _, male} -> getFemales(Rest, Acc);
		{user, _, _, female} -> getFemales(Rest, [User | Acc])
	end;

getFemales([], Acc) -> lists:reverse(Acc).


splitByAge(Users) -> splitByAge(Users, {[], []}).

splitByAge([], {Ls, Gt}) -> {lists:reverse(Ls), lists:reverse(Gt)};

splitByAge([User | Rest], {Ls, Gt}) ->
	{user, _, _, _, Age} = User,
	if Age < 18 -> splitByAge(Rest, {[User | Ls], Gt});
		 Age >= 18 -> splitByAge(Rest, {Ls, [User | Gt]})
	end.

