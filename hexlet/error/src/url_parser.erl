-module(url_parser).

-export([parse/1]).
-compile(export_all).

-spec parse(binary()) -> {ok, map()} | {error, term()}.
parse(URL) ->
  try
    {ok, URL2, State2} = get_protocol(URL, #{}),
    {ok, URL3, State3} = get_domain(URL2, State2),
    {ok, State4} = split_query(URL3, State3),
    {ok, State4}
  catch
    invalid_protocol -> {error, invalid_protocol};
    invalid_domain -> {error, invalid_domain}
  end.

get_protocol(URL, Map) ->
  case binary:split(URL, [<<"://">>]) of
    [Protocol, Rest] -> {ok, Rest, maps:put(protocol, Protocol, Map)};
    _ -> throw(invalid_protocol)
  end.

get_domain(URL, Map) ->
  [Domain, Rest] =
    case binary:split(URL, [<<"/">>]) of
      [D, R] -> [D, R];
      [<<>>] -> throw(invalid_domain);
      [<<"?", _/binary>>] -> throw(invalid_domain);
       [D] -> [D, <<>>]
    end,
  {ok, Rest, maps:put(domain, Domain, Map)}.

split_query(URL, Map) ->
  {Path, Query} =
    case binary:split(URL, [<<"?">>]) of
      [P, Q] -> {P, Q};
      [P] -> {P, <<>>}
    end,
  parse_path(Path, maps:put(query, Query, Map)).

parse_path(PathChunk, Map) ->
  Path = binary:split(PathChunk, [<<"/">>], [global]),
  NewState =
    try
      [YearB | [MonthB | [DateB | _]]] = Path,
      {Year,  []} = string:to_integer(binary_to_list(YearB)),
      {Month, []} = string:to_integer(binary_to_list(MonthB)),
      _ =
        if Month < 1; Month > 12 ->
          throw(bad_date);
          true -> 0
        end,
      {Date,  []} = string:to_integer(binary_to_list(DateB)),
      _ =
        if Date < 1; Date > 31->
          throw(bad_date);
          true -> 0
        end,
      maps:put(date, {Year, Month, Date}, Map)
    catch
      bad_date ->
        maps:put(date, undefined, Map);
      error:{badmatch, _} ->
        maps:put(date, undefined, Map)
    end,
  {ok, maps:put(path, [X || X <- Path, X =/= <<>>], NewState)}.
