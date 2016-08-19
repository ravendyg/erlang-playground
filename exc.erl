-module(exc).

-compile(export_all).

throws (F) ->
  try F() of
    _ -> ok
  catch
    throw:Throw -> {throw, caught, Throw}
  end.

errors (F) ->
  try F() of
    _ -> ok
  catch
    error:Error -> {error, caught, Error}
  end.

exits (F) ->
  try F() of
    _ -> ok
  catch
    exit:Exit -> {exit, caught, Exit}
  end.