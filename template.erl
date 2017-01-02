-module(template).

-export([parse/2]).

parse(Str, Data) when is_binary(Str) ->
  replace_elem(Str, maps:to_list(Data)).


replace_elem(Str, []) ->
  {_, PP4} = re:compile("{{[^}]*}}"),
  unicode:characters_to_binary(
    re:replace(Str, PP4, "", [global])
  );

replace_elem(Str, [{Key, Val} | Tail]) ->
  Sub = any_to_binary(Val),
  replace_elem(
    binary:replace(Str, wrap_template(Key), Sub), Tail
  ).

%  understood incorrectly <<param1>> as a first encountered
% wrap_template (Key, Str) ->
%   case binary:match(Key, <<"param">>) of
%     nomatch -> unicode:characters_to_binary(["{{", Key, "}}"]);
%     _ -> make_param_template(Str)
%   end.

wrap_template (Key) ->
  unicode:characters_to_binary(["{{", Key, "}}"]).

% make_param_template (Str) ->
%   case binary:split(Str, [<<"{{">>, <<"}}">>], [global]) of
%     [_, Key | _] -> unicode:characters_to_binary([<<"{{">>, Key, <<"}}">>]);
%     _            -> <<"">>
%   end.

any_to_binary (Val) ->
  unicode:characters_to_binary( any_to_list(Val) ).

any_to_list (Val) ->
  if is_integer(Val) -> integer_to_list(Val);
     is_float(Val)   -> float_to_list(Val);
     true            -> Val
  end.