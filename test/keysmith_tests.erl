-module(keysmith_tests).

-include_lib("eunit/include/eunit.hrl").

%--- Macros --------------------------------------------------------------------

-define(TS, 1724404226183).
-define(UUID_V7_RANDA, <<134, 2:4>>).
-define(UUID_V7_RANDB, <<227, 13, 114, 227, 245, 207, 207, 17:6>>).

-define(UUID_HEX_MATCH,
    <<_:8/bytes, $-, _:4/bytes, $-, _:4/bytes, $-, _:4/bytes, $-, _:12/bytes>>
).

%--- Tests ---------------------------------------------------------------------

uuid_format_hex_test_() ->
    {inparallel, [
        ?_assertMatch(?UUID_HEX_MATCH, keysmith:uuid(v4)),
        ?_assertMatch(?UUID_HEX_MATCH, keysmith:uuid(v7))
    ]}.

uuid_format_test_() ->
    {inparallel, [
        ?_assertEqual(
            binary:replace(
                keysmith:uuid({v7, ?TS, ?UUID_V7_RANDA, ?UUID_V7_RANDB}),
                <<$->>,
                <<>>,
                [global]
            ),
            keysmith:uuid({v7, ?TS, ?UUID_V7_RANDA, ?UUID_V7_RANDB}, hex_nodash)
        ),
        ?_assertEqual(
            keysmith:uuid({v7, ?TS, ?UUID_V7_RANDA, ?UUID_V7_RANDB}, bytes),
            keysmith:uuid({v7, ?TS, ?UUID_V7_RANDA, ?UUID_V7_RANDB}, binary)
        )
    ]}.

-dialyzer({nowarn_function, uuid_spec_invalid_test/0}).
uuid_spec_invalid_test() ->
    ?assertError(
        {invalid_uuid_spec, unsupported},
        keysmith:uuid(unsupported)
    ).

-dialyzer({nowarn_function, uuid_spec_format_test/0}).
uuid_spec_format_test() ->
    ?assertError(
        {invalid_uuid_format, unsupported},
        keysmith:uuid(v4, unsupported)
    ).

uuid_v4_test() ->
    ?assertMatch({v4, _, _, _}, keysmith:parse(uuid, keysmith:uuid(v4))),
    ?assertMatch(
        {v4, <<0:48>>, <<0:12>>, <<0:62>>},
        keysmith:parse(uuid, keysmith:uuid({v4, <<0:48>>, <<0:12>>, <<0:62>>}))
    ).

uuid_v7_test() ->
    Before = erlang:system_time(millisecond),
    V7Now = keysmith:parse(uuid, keysmith:uuid(v7)),
    After = erlang:system_time(millisecond),
    ?assertMatch({v7, _, _, _}, V7Now),
    {v7, TS, _, _} = V7Now,
    ?assert(Before =< TS andalso TS =< After).

uuid_v7_params_test_() ->
    {inparallel, [
        ?_assertMatch(
            {v7, 0, _, _},
            keysmith:parse(uuid, keysmith:uuid({v7, 0}))
        ),
        ?_assertMatch(
            {v7, 0, <<0:12>>, <<0:62>>},
            keysmith:parse(uuid, keysmith:uuid({v7, 0, <<0:12>>, <<0:62>>}))
        ),
        ?_assertMatch(
            {v7, ?TS, <<0:12>>, <<0:62>>},
            keysmith:parse(uuid, keysmith:uuid({v7, ?TS, <<0:12>>, <<0:62>>}))
        ),
        ?_assertMatch(
            {v7, 1, <<0:12>>, <<0:62>>},
            keysmith:parse(uuid, keysmith:uuid({v7, 1, <<0:12>>, <<0:62>>}))
        ),
        ?_assertMatch(
            {v7, 123, <<0:12>>, <<0:62>>},
            keysmith:parse(uuid, keysmith:uuid({v7, 123, <<0:12>>, <<0:62>>}))
        ),
        ?_assertMatch(
            {v7, 123, ?UUID_V7_RANDA, ?UUID_V7_RANDB},
            keysmith:parse(
                uuid, keysmith:uuid({v7, 123, ?UUID_V7_RANDA, ?UUID_V7_RANDB})
            )
        )
    ]}.

-dialyzer({nowarn_function, uuid_parse_invalid_test/0}).
uuid_parse_invalid_test() ->
    ?assertError(
        {invalid_id, uuid, <<"foo">>}, keysmith:parse(uuid, <<"foo">>)
    ).

type_id_test_() ->
    {inparallel, [
        ?_assertMatch(
            {user, {v7, _, _, _}},
            keysmith:parse(type_id, keysmith:type_id(user))
        ),
        ?_assertMatch(
            {user, {v7, 0, <<0:12>>, <<0:62>>}},
            keysmith:parse(
                type_id, keysmith:type_id(user, {v7, 0, <<0:12>>, <<0:62>>})
            )
        ),
        ?_assertMatch(
            {user, {v7, 123, <<0:12>>, <<0:62>>}},
            keysmith:parse(
                type_id, keysmith:type_id(user, {v7, 123, <<0:12>>, <<0:62>>})
            )
        )
    ]}.

type_id_parse_uuid_test() ->
    UUID = keysmith:uuid(v4),
    ?assertError({invalid_id, type_id, UUID}, keysmith:parse(type_id, UUID)),
    ?assertError(
        {invalid_id, type_id, <<"j7ZfQIgSJtb2">>},
        keysmith:parse(type_id, <<"j7ZfQIgSJtb2">>)
    ),
    ?assertError(
        {invalid_id, type_id, <<"j7ZfQIgSJtb2_01j8m6vs74fj3r46whh9xs6k1j">>},
        keysmith:parse(type_id, <<"j7ZfQIgSJtb2_01j8m6vs74fj3r46whh9xs6k1j">>)
    ).

type_id_parse_uuid_strict_test_() ->
    {inparallel, [
        fun() ->
            TypeID = <<"user_0000000000800800000", C, "0000000">>,
            ?assertError(
                {invalid_id, type_id, TypeID}, keysmith:parse(type_id, TypeID)
            )
        end
     || C <- lists:seq($A, $Z) ++ ":;<=>?@[\]^_` ilou"
    ]}.

% type_id_tolerant_decode_test() ->
%     ?assertEqual(
%         keysmith:parse(type_id, <<"user_oij5Z74RK2EM6uUSM8IXT6AYO4">>, #{base32_decoding => tolerant}),
%         keysmith:parse(type_id, <<"user_01j5z74rk2em6vvsm81xt6ay04">>),
%     ).

cb32_test_() ->
    {inparallel, [
        ?_assertEqual(V, keysmith:cb32_decode(keysmith:cb32_encode(V)))
     || V <- [
            <<0:130>>,
            <<0:2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1>>,
            <<0:2, 3, 34, 252, 234, 158, 42, 127, 191, 141, 202, 239, 133, 169,
                0, 81, 176>>
        ]
    ]}.

-dialyzer({nowarn_function, parse_invalid_type_test/0}).
parse_invalid_type_test() ->
    ?assertError({invalid_id_type, foo}, keysmith:parse(foo, <<"foo">>)).

spec_invalid_test_() ->
    {ok, JSON} = file:read_file("test/fixtures/type_id/spec/invalid.json"),
    IDs = json:decode(JSON),
    [
        {Desc,
            ?_assertError(
                {invalid_id, type_id, ID},
                keysmith:parse(type_id, ID)
            )}
     || #{~"name" := _Name, ~"typeid" := ID, ~"description" := Desc} <- IDs
    ].
