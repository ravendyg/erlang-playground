-module(short_link).

-compile(export_all).

%%% module API

init() ->
    %% init randomizer
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exs64, {A,B,C}),
    {gb_trees:empty(), gb_trees:empty()}.


create_short(LongLink, State) ->
  {ShortKeyTree, LongKeyTree} = State,
  case gb_trees:lookup(LongLink, LongKeyTree) of
    {value, Short} -> {Short, State};
    none ->
      Short = "http://hexlet.io/" ++ rand_str(10),
      {
        Short,
        {
          gb_trees:enter(Short, LongLink, ShortKeyTree),
          gb_trees:enter(LongLink, Short, LongKeyTree)
        }
      }
  end.

get_long(ShortLink, State) ->
    {ShortKeyTree, _} = State,
    case gb_trees:lookup(ShortLink, ShortKeyTree) of
      {value, LongLink} -> {ok, LongLink};
      none              -> {error, not_found}
    end.


%% generates random string of chars [a-zA-Z0-9]
rand_str(Length) ->
    lists:map(fun(Char) when Char > 83 -> Char + 13;
                 (Char) when Char > 57 -> Char + 7;
                 (Char) -> Char
              end,
              [crypto:rand_uniform(48, 110) || _ <- lists:seq(1, Length)]).
