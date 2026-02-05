-module(cs_json).

%% elvis:ignore [no_throw, invalid_dynamic_call]
%% Throws are used for error propagation with context.
%% Dynamic calls are intentional for Thrift struct reflection.

-export([decode/1]).
-export([encode/1]).

-export([json_to_term/2]).
-export([term_to_json/2]).

-export_type([thrift_type/0]).

-type thrift_type() ::
    base_type()
    | collection_type()
    | enum_type()
    | struct_type().

-type base_type() ::
    bool
    | byte
    | double
    | i8
    | i16
    | i32
    | i64
    | string.

-type collection_type() ::
    {list, thrift_type()}
    | {set, thrift_type()}
    | {map, thrift_type(), thrift_type()}.

-type enum_type() ::
    {enum, type_ref()}.

-type struct_type() ::
    {struct, struct_flavor(), type_ref()}.

-type struct_flavor() :: struct | union | exception.

-type type_ref() :: {module(), Name :: atom()}.

%%

-spec decode(string() | binary()) -> jsone:json_value().
decode(S) when is_binary(S) ->
    jsone:decode(S, [{keys, binary}, {object_format, proplist}]);
decode(S) when is_list(S) ->
    case unicode:characters_to_binary(S) of
        Bin when is_binary(Bin) -> decode(Bin);
        {error, _, _} -> erlang:error(badarg);
        {incomplete, _, _} -> erlang:error(badarg)
    end.

-spec encode(jsone:json_value()) -> binary().
encode(J) ->
    jsone:encode(J, [native_utf8, native_forward_slash]).

%%

-define(IS_INTEGER(T), (T == byte orelse T == i8 orelse T == i16 orelse T == i32 orelse T == i64)).
-define(IS_NUMBER(T), (?IS_INTEGER(T) orelse T == double)).
-define(IS_SCALAR(T), (?IS_NUMBER(T) orelse T == string orelse element(1, T) == enum)).

-spec json_to_term(jsone:json_value(), thrift_type()) -> term().
json_to_term(Json, Type) ->
    json_to_term(Json, Type, []).

json_to_term(Json, Type, Stack) ->
    try
        do_json_to_term(Json, Type, Stack)
    catch
        error:missing ->
            erlang:throw({missing, lists:reverse(Stack)});
        error:_ ->
            erlang:throw({invalid, lists:reverse(Stack), Json, Type})
    end.

do_json_to_term(undefined, {optional, _Type}, _Stack) ->
    undefined;
do_json_to_term(undefined, {required, _Type}, _Stack) ->
    erlang:error(missing);
%% NOTE In case of undefined field flag (nor optional nor required) like in
%%
%%   struct DummyRef {
%%       1: base.ID id
%%   }
%%
%% we interpret it as optional.
do_json_to_term(Json, {undefined, Type}, Stack) ->
    do_json_to_term(Json, {optional, Type}, Stack);
do_json_to_term(Json, {Req, Type}, Stack) when Req == optional; Req == required ->
    do_json_to_term(Json, Type, Stack);
do_json_to_term(Json, {list, Type}, Stack) when is_list(Json) ->
    [json_to_term(T, Type, [N | Stack]) || {N, T} <- enumerate(0, Json)];
do_json_to_term(Json, {set, Type}, Stack) when is_list(Json) ->
    ordsets:from_list(do_json_to_term(Json, {list, Type}, Stack));
do_json_to_term([{}], {map, KType, _VType}, _Stack) when ?IS_SCALAR(KType) ->
    #{};
do_json_to_term(Json, {map, KType, VType}, Stack) when is_list(Json), ?IS_SCALAR(KType) ->
    lists:foldl(
        fun({K, V}, A) ->
            A#{
                json_propkey_to_term(K, KType, [key, K | Stack]) =>
                    json_to_term(V, VType, [value, K | Stack])
            }
        end,
        #{},
        Json
    );
do_json_to_term(Json, {map, KType, VType}, Stack) when is_list(Json) ->
    lists:foldl(
        fun(Pair, A) ->
            K = getv(<<"key">>, Pair),
            V = getv(<<"value">>, Pair),
            A#{
                json_to_term(K, KType, [key, K | Stack]) =>
                    json_to_term(V, VType, [value, K | Stack])
            }
        end,
        #{},
        Json
    );
do_json_to_term(Json, {enum, {Mod, Name}}, Stack) when is_atom(Mod), is_atom(Name) ->
    do_json_to_term(Json, Mod:enum_info(Name), Stack);
do_json_to_term(Json, {enum, Fields}, _Stack) when is_list(Fields), is_binary(Json) ->
    V = binary_to_atom(Json, utf8),
    case lists:keyfind(V, 1, Fields) of
        {V, _} ->
            V;
        false ->
            erlang:error(badarg)
    end;
do_json_to_term(Json, {struct, union, {Mod, Name}}, Stack) when is_atom(Mod), is_atom(Name) ->
    {struct, union, StructDef} = Mod:struct_info(Name),
    json_to_union(Json, StructDef, Stack);
do_json_to_term(Json, {struct, _, {Mod, Name}}, Stack) when is_atom(Mod), is_atom(Name) ->
    {struct, _, StructDef} = Mod:struct_info(Name),
    json_to_struct(Json, StructDef, Mod:record_name(Name), Stack);
do_json_to_term(Json, string, _Stack) when is_binary(Json) ->
    Json;
do_json_to_term([{_, _} | _] = Json, string, _Stack) ->
    CType = getv(<<"content_type">>, Json),
    Content = getv(<<"content">>, Json),
    json_content_to_string(CType, Content);
do_json_to_term(Json, bool, _Stack) when is_boolean(Json) ->
    Json;
do_json_to_term(Json, double, _Stack) when is_number(Json) ->
    float(Json);
do_json_to_term(Json, Type, _Stack) when
    Type == i8; Type == byte, is_integer(Json), Json >= -(1 bsl 7), Json < (1 bsl 7)
->
    Json;
do_json_to_term(Json, i16, _Stack) when is_integer(Json), Json >= -(1 bsl 15), Json < (1 bsl 15) ->
    Json;
do_json_to_term(Json, i32, _Stack) when is_integer(Json), Json >= -(1 bsl 31), Json < (1 bsl 31) ->
    Json;
do_json_to_term(Json, i64, _Stack) when is_integer(Json), Json >= -(1 bsl 63), Json < (1 bsl 63) ->
    Json;
do_json_to_term(_Json, _Type, _Stack) ->
    erlang:error(badarg).

json_propkey_to_term(P, string = Type, Stack) ->
    do_json_to_term(P, Type, Stack);
json_propkey_to_term(P, {enum, _} = Type, Stack) ->
    do_json_to_term(P, Type, Stack);
json_propkey_to_term(P, Type, Stack) when ?IS_INTEGER(Type) ->
    do_json_to_term(binary_to_integer(P), Type, Stack);
json_propkey_to_term(P, double = Type, Stack) ->
    do_json_to_term(
        try
            binary_to_float(P)
        catch
            error:badarg -> binary_to_integer(P)
        end,
        Type,
        Stack
    ).

json_to_struct(Json, StructDef, RecordName, Stack) when is_list(Json) ->
    list_to_tuple([
        RecordName
        | lists:map(
            fun({_N, Req, Type, Fn, Def}) ->
                FJson = getv(atom_to_binary(Fn, utf8), Json, Def),
                json_to_term(FJson, {Req, Type}, [Fn | Stack])
            end,
            StructDef
        )
    ]).

json_to_union([{FnBin, Json}], StructDef, Stack) ->
    {_N, _Req, Type, Fn, _Def} = lists:keyfind(binary_to_atom(FnBin, utf8), 4, StructDef),
    {Fn, json_to_term(Json, Type, [Fn | Stack])}.

json_content_to_string(<<"base64">>, Content) ->
    base64:decode(Content).

%%

-spec term_to_json(term(), thrift_type()) -> jsone:json_value().
term_to_json(Term, Type) ->
    term_to_json(Term, Type, []).

term_to_json(Term, {list, Type}, Stack) when is_list(Term) ->
    [term_to_json(T, Type, [N | Stack]) || {N, T} <- enumerate(0, Term)];
term_to_json(Term, {set, Type}, Stack) ->
    term_to_json(ordsets:to_list(Term), {list, Type}, Stack);
term_to_json(Term, {map, KType, VType}, Stack) when is_map(Term), ?IS_SCALAR(KType) ->
    maps:fold(
        fun(K, V, A) ->
            [{genlib:to_binary(K), term_to_json(V, VType, [value, V | Stack])} | A]
        end,
        [],
        Term
    );
term_to_json(Term, {map, KType, VType}, Stack) when is_map(Term) ->
    maps:fold(
        fun(K, V, A) ->
            [
                [
                    {<<"key">>, term_to_json(K, KType, [key, K | Stack])},
                    {<<"value">>, term_to_json(V, VType, [value, V | Stack])}
                ]
                | A
            ]
        end,
        [],
        Term
    );
term_to_json(Term, {struct, union, {Mod, Name}}, Stack) when is_atom(Mod), is_atom(Name) ->
    {struct, _, StructDef} = Mod:struct_info(Name),
    union_to_json(Term, StructDef, Stack);
term_to_json(Term, {struct, _, {Mod, Name}}, Stack) when
    is_atom(Mod), is_atom(Name), is_tuple(Term)
->
    {struct, _, StructDef} = Mod:struct_info(Name),
    struct_to_json(Term, StructDef, Stack);
term_to_json(Term, {enum, _}, _Stack) when is_atom(Term) ->
    Term;
term_to_json(Term, Type, _Stack) when is_integer(Term), ?IS_INTEGER(Type) ->
    Term;
term_to_json(Term, double, _Stack) when is_number(Term) ->
    float(Term);
term_to_json(Term, string, _Stack) when is_binary(Term) ->
    case is_printable_string(Term) of
        true ->
            Term;
        false ->
            term_to_json_content(Term)
    end;
term_to_json(Term, bool, _Stack) when is_boolean(Term) ->
    Term;
term_to_json(Term, Type, _Stack) ->
    erlang:error({badarg, Term, Type}).

union_to_json({Fn, Term}, StructDef, Stack) ->
    {_N, _Req, Type, Fn, _Def} = lists:keyfind(Fn, 4, StructDef),
    [{Fn, term_to_json(Term, Type, [Fn | Stack])}].

struct_to_json(Struct, StructDef, Stack) ->
    [_ | Fields] = tuple_to_list(Struct),
    lists:foldr(
        fun
            ({undefined, _}, A) ->
                A;
            ({Term, {_N, _Req, Type, Fn, _Def}}, A) ->
                [{Fn, term_to_json(Term, Type, [Fn | Stack])} | A]
        end,
        [],
        lists:zip(Fields, StructDef)
    ).

term_to_json_content(Term) ->
    term_to_json_content(<<"base64">>, base64:encode(Term)).

term_to_json_content(CType, Term) ->
    [
        {<<"content_type">>, CType},
        {<<"content">>, Term}
    ].

%%

enumerate(_, []) ->
    [];
enumerate(N, [H | T]) ->
    [{N, H} | enumerate(N + 1, T)].

getv(Key, Opts) ->
    getv(Key, Opts, undefined).

getv(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, Value} -> Value;
        false -> Default
    end.

is_printable_string(<<>>) ->
    true;
is_printable_string(S) ->
    case string:next_codepoint(S) of
        [C | Rest] ->
            printable_unicode_char(C) andalso is_printable_string(Rest);
        {error, _} ->
            false
    end.

printable_unicode_char(C) when is_integer(C), C >= $\040, C =< $\176 ->
    true;
printable_unicode_char(C) when
    is_integer(C), C >= 16#A0, C < 16#D800;
    is_integer(C), C > 16#DFFF, C < 16#FFFE;
    is_integer(C), C > 16#FFFF, C =< 16#10FFFF
->
    true;
printable_unicode_char($\n) ->
    true;
printable_unicode_char($\r) ->
    true;
printable_unicode_char($\t) ->
    true;
printable_unicode_char($\v) ->
    true;
printable_unicode_char($\b) ->
    true;
printable_unicode_char($\f) ->
    true;
printable_unicode_char($\e) ->
    true;
printable_unicode_char(_) ->
    false.
