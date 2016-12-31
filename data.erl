-module(data).

-include("records.hrl").

-compile(export_all).

makeRobot () ->
  #robot{ name="Mechatron",
          type=handmade,
          details=["Moved by smbd inside"]}.

carFactory(CorpName) -> #robot{ name=CorpName, hobbies="building cars" }.

repairman( Rob ) ->
  Details = Rob#robot.details,
  NewRob = Rob#robot{ details=[ "repaired by" | Details ] },
  { repaired, NewRob }.

adminPanel( #user{ name=Name, group=admin } ) ->
  Name ++ " is allowed";
adminPanel( #user{ name=Name } ) ->
  Name ++ " is not allowed".


% included() -> #included{ someField="Some value" }.

qu() -> queue:new().

dolphin1() ->
  receive
    doAFlip ->
      io:format("no!~n");
    fish ->
      io:format("so long~n");
     _   ->
      io:format("else~n")
  end.

dilphin2() ->
  receive
    { From, doAFlip } ->
      From ! "no";
    { From, fish }    ->
      From ! "so long";
    { From, _ }       ->
      From ! "else";
    _                 ->
      io:format("no pid ~n")
  end.

dolphin3() ->
  receive
    { From, doAFlip } ->
      From ! "no",
      dolphin3();
    { From, fish }    ->
      From ! "so long";
    { From, _ }       ->
      From ! "else",
      dolphin3();
    _                 ->
      io:format("no pid ~n"),
      dolphin3()
  end.