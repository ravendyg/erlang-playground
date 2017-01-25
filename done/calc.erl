-module(calc).

-compile(export_all).


run (Input) -> run(Input, " ").

run (Input, Separator) ->
  In = string:tokens(Input, Separator),
  [Res] = lists:foldl( fun parse/2, [], In),
  Res.


parse("+", [First, Second | Rest]) -> [Second + First | Rest];
parse("-", [First, Second | Rest]) -> [Second - First | Rest];
parse("*", [First, Second | Rest]) -> [Second * First | Rest];
parse("/", [First, Second | Rest]) ->
  case First of
    0 -> throw("Division by zero");
    _ -> [Second / First | Rest]
  end;
parse("^", [First, Second | Rest]) -> [math:pow(Second, First) | Rest];
parse("ln", [First | Rest]) -> [math:log(First) | Rest];
parse("log", [First | Rest]) -> [math:log10(First) | Rest];

parse(Operand, Stack) ->
  {Val, _} = string:to_float(Operand),
  case Val of
    error ->
      {Val2, _} = string:to_integer(Operand),
      case Val2 of
        error -> throw("Unknown operand " ++ Operand);
          _   -> [Val2 | Stack]
      end;
     _  -> [Val | Stack]
  end.

% calc:run("10 12 + 2 / 8 + 3 * 5 /"). -> 11.4

cl_test () ->
  5 = run("2 3 +"),
  87 = run("90 3 -"),
  -2.0 = run("10 4 3 + 2 * - 2 /"),
  ok = try
    run(" 90 34 12 33 55 66 + * - +")
  catch
    error:{badmatch,[_|_]} -> ok
  end,
  ok.