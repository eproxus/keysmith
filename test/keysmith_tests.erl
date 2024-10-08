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
        ?_assertMatch(?UUID_HEX_MATCH, keysmith:uuid(4)),
        ?_assertMatch(?UUID_HEX_MATCH, keysmith:uuid(7))
    ]}.

uuid_format_test_() ->
    {inparallel, [
        ?_assertEqual(
            binary:replace(
                keysmith:uuid({7, ?TS, ?UUID_V7_RANDA, ?UUID_V7_RANDB}),
                <<$->>,
                <<>>,
                [global]
            ),
            keysmith:uuid({7, ?TS, ?UUID_V7_RANDA, ?UUID_V7_RANDB}, hex_nodash)
        ),
        ?_assertEqual(
            keysmith:uuid({7, ?TS, ?UUID_V7_RANDA, ?UUID_V7_RANDB}, bytes),
            keysmith:uuid({7, ?TS, ?UUID_V7_RANDA, ?UUID_V7_RANDB}, binary)
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
        keysmith:uuid(4, unsupported)
    ).

uuid_v4_random_test() ->
    Hex = keysmith:uuid(4),
    Bin = binary:decode_hex(binary:replace(Hex, ~"-", ~"", [global])),
    ?assertMatch(
        #{
            var := rfc,
            ver := 4,
            val := #{rand_a := _, rand_b := _, rand_c := _},
            bin := Bin,
            hex := Hex
        },
        keysmith:parse(uuid, Hex)
    ).

% uuid_v1_random_test() ->
%     Random = keysmith:uuid(1),
%     Raw = binary:decode_hex(binary:replace(Random, ~"-", ~"", [global])),
%     ?assertMatch(
%         #{
%             var := rfc,
%             ver := 1,
%             value := #{
%                 time := _,
%                 date_time := {{_, _, _}, {_, _, _}}
%                 node := _
%             },
%             raw := Raw,
%             hex := Random
%         },
%         keysmith:parse(uuid, Random)
%     ).

uuid_v4_fixed_test() ->
    ?assertMatch(
        #{
            var := rfc,
            ver := 4,
            val := #{rand_a := <<0:48>>, rand_b := <<0:12>>, rand_c := <<0:62>>}
        },
        keysmith:parse(uuid, keysmith:uuid({4, <<0:48>>, <<0:12>>, <<0:62>>}))
    ).

uuid_v7_test() ->
    Before = erlang:system_time(millisecond),
    Hex = keysmith:uuid(7),
    After = erlang:system_time(millisecond),
    V7Now = keysmith:parse(uuid, Hex),
    Bin = binary:decode_hex(binary:replace(Hex, ~"-", ~"", [global])),
    ?assertMatch(
        #{
            var := rfc,
            ver := 7,
            bin := Bin,
            hex := Hex,
            val := #{unix_ts := {millisecond, _}, rand_a := _, rand_b := _}
        },
        V7Now
    ),
    #{val := #{unix_ts := {millisecond, TS}}} = V7Now,
    ?assert(Before =< TS andalso TS =< After).

uuid_v7_params_test_() ->
    {inparallel, [
        ?_assertMatch(
            #{
                var := rfc,
                ver := 7,
                val := #{
                    unix_ts := {millisecond, 0},
                    rand_a := _,
                    rand_b := _
                }
            },
            keysmith:parse(uuid, keysmith:uuid({7, 0}))
        ),
        ?_assertMatch(
            #{
                var := rfc,
                ver := 7,
                val := #{
                    unix_ts := {millisecond, 0},
                    rand_a := <<0:12>>,
                    rand_b := <<0:62>>
                }
            },
            keysmith:parse(uuid, keysmith:uuid({7, 0, <<0:12>>, <<0:62>>}))
        ),
        ?_assertMatch(
            #{
                var := rfc,
                ver := 7,
                val := #{
                    unix_ts := {millisecond, ?TS},
                    rand_a := <<0:12>>,
                    rand_b := <<0:62>>
                }
            },
            keysmith:parse(uuid, keysmith:uuid({7, ?TS, <<0:12>>, <<0:62>>}))
        ),
        ?_assertMatch(
            #{
                var := rfc,
                ver := 7,
                val := #{
                    unix_ts := {millisecond, 1},
                    rand_a := <<0:12>>,
                    rand_b := <<0:62>>
                }
            },
            keysmith:parse(uuid, keysmith:uuid({7, 1, <<0:12>>, <<0:62>>}))
        ),
        ?_assertMatch(
            #{
                var := rfc,
                ver := 7,
                val := #{
                    unix_ts := {millisecond, 123},
                    rand_a := <<0:12>>,
                    rand_b := <<0:62>>
                }
            },
            keysmith:parse(uuid, keysmith:uuid({7, 123, <<0:12>>, <<0:62>>}))
        ),
        ?_assertMatch(
            #{
                var := rfc,
                ver := 7,
                val := #{
                    unix_ts := {millisecond, 123},
                    rand_a := ?UUID_V7_RANDA,
                    rand_b := ?UUID_V7_RANDB
                }
            },
            keysmith:parse(
                uuid, keysmith:uuid({7, 123, ?UUID_V7_RANDA, ?UUID_V7_RANDB})
            )
        )
    ]}.

-dialyzer({nowarn_function, uuid_parse_invalid_test/0}).
uuid_parse_invalid_test() ->
    ?assertError(
        {invalid_id, uuid, <<"foo">>}, keysmith:parse(uuid, <<"foo">>)
    ).

uuid_nil_test_() ->
    {inparallel, [
        ?_assertEqual(
            ~"00000000-0000-0000-0000-000000000000", keysmith:uuid(nil)
        ),
        ?_assertEqual(
            ~"00000000000000000000000000000000", keysmith:uuid(nil, hex_nodash)
        ),
        ?_assertEqual(
            <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
            keysmith:uuid(nil, binary)
        ),
        ?_assertEqual(
            #{
                var => {reserved, ncs},
                ver => 0,
                bin => <<0:128>>,
                hex => ~"00000000-0000-0000-0000-000000000000",
                val => nil
            },
            keysmith:parse(uuid, keysmith:uuid(nil))
        )
    ]}.

uuid_max_test_() ->
    {inparallel, [
        ?_assertEqual(
            ~"ffffffff-ffff-ffff-ffff-ffffffffffff", keysmith:uuid(max)
        ),
        ?_assertEqual(
            ~"ffffffffffffffffffffffffffffffff", keysmith:uuid(max, hex_nodash)
        ),
        ?_assertEqual(
            <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF:128>>,
            keysmith:uuid(max, binary)
        ),
        ?_assertEqual(
            #{
                var => {reserved, future},
                ver => 15,
                bin => <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF:128>>,
                hex => ~"ffffffff-ffff-ffff-ffff-ffffffffffff",
                val => max
            },
            keysmith:parse(uuid, keysmith:uuid(max))
        )
    ]}.

type_id_test_() ->
    {inparallel, [
        ?_assertMatch(
            {type_id, user, #{
                var := rfc,
                ver := 7,
                val := #{
                    unix_ts := {millisecond, _},
                    rand_a := _,
                    rand_b := _
                }
            }},
            keysmith:parse(type_id, keysmith:type_id(user))
        ),
        ?_assertEqual(
            {type_id, user, #{
                var => rfc,
                ver => 7,
                bin => <<0, 0, 0, 0, 0, 0, 112, 0, 128, 0, 0, 0, 0, 0, 0, 0>>,
                hex => <<"00000000-0000-7000-8000-000000000000">>,
                val => #{
                    unix_ts => {millisecond, 0},
                    rand_a => <<0:12>>,
                    rand_b => <<0:62>>
                }
            }},
            keysmith:parse(
                type_id, keysmith:type_id(user, {7, 0, <<0:12>>, <<0:62>>})
            )
        ),
        ?_assertEqual(
            {type_id, user, #{
                var => rfc,
                ver => 7,
                bin => <<0, 0, 0, 0, 0, 123, 112, 0, 128, 0, 0, 0, 0, 0, 0, 0>>,
                hex => <<"00000000-007b-7000-8000-000000000000">>,
                val => #{
                    unix_ts => {millisecond, 123},
                    rand_a => <<0:12>>,
                    rand_b => <<0:62>>
                }
            }},
            keysmith:parse(
                type_id, keysmith:type_id(user, {7, 123, <<0:12>>, <<0:62>>})
            )
        )
    ]}.

type_id_parse_uuid_test_() ->
    UUID = keysmith:uuid(4),
    {inparallel, [
        ?_assertError(
            {invalid_id, type_id, UUID}, keysmith:parse(type_id, UUID)
        ),
        ?_assertError(
            {invalid_id, type_id, ~"j7ZfQIgSJtb2"},
            keysmith:parse(type_id, ~"j7ZfQIgSJtb2")
        ),
        ?_assertError(
            {invalid_id, type_id, ~"j7ZfQIgSJtb2_01j8m6vs74fj3r46whh9xs6k1j"},
            keysmith:parse(type_id, ~"j7ZfQIgSJtb2_01j8m6vs74fj3r46whh9xs6k1j")
        ),
        ?_assertError(
            {invalid_id, type_id, ~"80000000000000000000000000"},
            keysmith:parse(type_id, ~"80000000000000000000000000")
        )
    ]}.

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

-dialyzer({nowarn_function, parse_invalid_format_test/0}).
parse_invalid_format_test() ->
    ?assertError({invalid_format, foo}, keysmith:parse(foo, <<"foo">>)).

spec_valid_test_() ->
    {ok, JSON} = file:read_file("test/fixtures/type_id/spec/valid.json"),
    IDs = json:decode(JSON),
    [
        {Name, fun() ->
            ExpectedUUID = keysmith:parse(uuid, UUID),
            ExpectedTypeID =
                case Prefix of
                    ~"" -> {type_id, ExpectedUUID};
                    Prefix -> {type_id, binary_to_atom(Prefix), ExpectedUUID}
                end,
            ?assertEqual(ExpectedTypeID, keysmith:parse(type_id, ID)),
            ?assertEqual(UUID, keysmith:uuid(ExpectedUUID))
        end}
     || #{
            ~"name" := Name,
            ~"typeid" := ID,
            ~"prefix" := Prefix,
            ~"uuid" := UUID
        } <- IDs
    ].

spec_invalid_test_() ->
    {ok, JSON} = file:read_file("test/fixtures/type_id/spec/invalid.json"),
    IDs = json:decode(JSON),
    [
        {Desc,
            ?_assertError(
                {invalid_id, type_id, ID},
                keysmith:parse(type_id, ID)
            )}
     || #{
            ~"name" := _Name,
            ~"typeid" := ID,
            ~"description" := Desc
        } <- IDs
    ].
