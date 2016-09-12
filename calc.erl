-module(calc).

-compile(export_all).


run (Input) -> run(Input, " ").

run (Input, Separator) ->
  In = string:tokens(Input, Separator),
  [Res] = lists:foldl( fun parse/2, [], In),
  Res.


parse(Operand, Stack) ->
  {Val, _} = string:to_integer(Operand),
  case Val of
    error ->
      [First | [Second | Rest]] = Stack,
      case Operand of
        "+" -> [Second + First | Rest];
        "-" -> [Second - First | Rest];
        "*" -> [Second * First | Rest];
        "/" ->
          case First of
            0 -> throw("Division by zero");
            _ -> [Second / First | Rest]
          end;
         _  -> throw("Unexpected operand: " ++ Operand)
      end;
     _  -> [Val | Stack]
  end.

% calc:run("10 12 + 2 / 8 + 3 * 5 /"). -> 11.4