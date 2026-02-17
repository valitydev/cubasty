-module(cs_pagination).

-export([encode/1, decode/1, decode/2]).

-spec encode(non_neg_integer()) -> binary().
encode(Offset) when is_integer(Offset), Offset >= 0 ->
    base64:encode(integer_to_binary(Offset)).

-spec decode(binary() | undefined) -> non_neg_integer().
decode(Token) ->
    decode(Token, 0).

-spec decode(binary() | undefined, non_neg_integer()) -> non_neg_integer().
decode(undefined, Default) ->
    Default;
decode(Token, Default) when is_binary(Token) ->
    try
        binary_to_integer(base64:decode(Token))
    catch
        _:_ -> Default
    end.
