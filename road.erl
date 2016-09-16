-module(road).

-compile(export_all).


main ([FileName]) ->
  % File = "road.txt",
  RawData = readData( FileName ),
  Data = groupVals( RawData, [] ),
  io:format( "~p~n", [optimalPath( Data )] ),
  erlang:halt(0).
  % Data.

%% readData
readData (Path) ->
  {Res, Binary} = file:read_file(Path),
  case Res of
    ok -> Binary;
    _  -> throw( Binary )
  end,
  parseInput( Binary ).


%% parseInput
parseInput (Binary) when is_binary(Binary) ->
  parseInput( binary_to_list(Binary) );

parseInput (Binary) when is_list(Binary) ->
  [ list_to_integer(X) || X <- string:tokens( Binary, "\n\t\r ")].


%% groupVals
groupVals ([], Acc) -> lists:reverse( Acc );

groupVals ([A, B, C | Rest], Acc) ->
  groupVals( Rest, [{A,B,C} | Acc] ).

%% shortestStep
shortestStep ({A,B,X}, {{DistA,PathA}, {DistB,PathB}}) ->
  OptA1 = {DistA + A, [{a,A} | PathA]},
  OptA2 = {DistB + B + X, [{x,X}, {b,B} | PathB]},
  OptB1 = {DistB + B, [{b,B} | PathB]},
  OptB2 = {DistA + A + X, [{x,X}, {a,A} | PathA]},
  {erlang:min( OptA1, OptA2 ), erlang:min( OptB1, OptB2 )}.

  %% optimalPath
  optimalPath (Map) ->
    {A,B} = lists:foldl( fun shortestStep/2, {{0,[]}, {0,[]}}, Map),
    {_Dist,Path} =
      if hd(element(2,A)) =/= {x,0} -> A;
         hd(element(2,B)) =/= {x,0} -> B
      end,
    lists:reverse(Path).