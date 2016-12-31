-module(champ_pairs).

-export([make_pairs/2]).
-include_lib("eunit/include/eunit.hrl").

make_pairs(Team1, Team2) ->
	{team, _, Players1} = Team1,
	{team, _, Players2} = Team2,
	lists:foldr(
		fun ({player, Name, _, Rating, _}, List) ->
			[{Name, Name2} || {player, Name2, _, Rating2, _} <- Players2, Rating + Rating2 > 600] ++ List
		end,
		[],
		Players1
	).


make_pairs_test() ->
    [T1, T2, T3, T4, _] = champ:sample_champ(),
    ?assertEqual([{"Big Bull","Lazy Horse"},
                  {"Big Bull","Sleepy Horse"},
                  {"Big Bull","Horse Doors"},
                  {"Big Bull","Rainbow"},
                  {"Big Bull","HoHoHorse"},
                  {"Big Bull","Pony"},
                  {"Big Bull","Hippo"},
                  {"Big Bull","Hop-Hop"},
                  {"Small Bull","Lazy Horse"},
                  {"Bull Dog","Lazy Horse"}],
                 make_pairs(T1, T2)),
    ?assertEqual([{"Lazy Horse","Light Speed Cow"},
                  {"Lazy Horse","Jumping Cow"},
                  {"Lazy Horse","Cow Flow"}],
                 make_pairs(T2, T3)),
    ?assertEqual([{"Ben The Hen","Light Speed Cow"},
                  {"Ben The Hen","Jumping Cow"},
                  {"Ben The Hen","Cow Flow"},
                  {"Hen Hen","Jumping Cow"},
                  {"Hen Hen","Cow Flow"},
                  {"Son of Hen","Boom! Cow"},
                  {"Son of Hen","Light Speed Cow"},
                  {"Son of Hen","Jumping Cow"},
                  {"Son of Hen","Cow Flow"}],
                 make_pairs(T4, T3)),
    ok.
