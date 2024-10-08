-module(prop_keysmith).
-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(UUID_MAP_V7(TS, RandA, RandB), #{
    var := rfc,
    ver := 7,
    val := #{
        unix_ts := {millisecond, TS},
        rand_a := RandA,
        rand_b := RandB
    }
}).

prop_uuid_v7(doc) -> "UUID v7 should be encodable and parseable";
prop_uuid_v7(opts) -> [{numtests, 100_000}].
prop_uuid_v7() ->
    ?FORALL(
        UUID,
        uuid_v7(),
        begin
            {7, TS, RandA, RandB} = UUID,
            ?assertMatch(
                ?UUID_MAP_V7(TS, RandA, RandB),
                keysmith:parse(uuid, keysmith:uuid(UUID))
            ),
            true
        end
    ).

prop_type_id(doc) -> "TypeID should be encodable and parseable";
prop_type_id(opts) -> [{numtests, 100_000}].
prop_type_id() ->
    ?FORALL(
        UUID,
        uuid_v7(),
        begin
            TypeID = keysmith:type_id(user, UUID),
            ?WHENFAIL(
                io:format("TypeID = ~p~n", [TypeID]),
                begin
                    {7, TS, RandA, RandB} = UUID,
                    ?assertMatch(
                        {type_id, user, ?UUID_MAP_V7(TS, RandA, RandB)},
                        keysmith:parse(type_id, TypeID)
                    ),
                    true
                end
            )
        end
    ).

prop_cb32(doc) -> "Crockford's Base 32 should be encodable and parseable";
prop_cb32(opts) -> [{numtests, 100_000}].
prop_cb32() ->
    ?FORALL(
        Bitstring,
        bitstring(130),
        begin
            Encoded = keysmith:cb32_encode(Bitstring),
            ?WHENFAIL(
                io:format("Encoded = ~p~n", [Encoded]),
                Bitstring =:= keysmith:cb32_decode(Encoded)
            )
        end
    ).

prop_uuid_random(doc) ->
    "Any random 128-bit binary should be encodable and parseable as UUID";
prop_uuid_random(opts) ->
    [{numtests, 100_000}].
prop_uuid_random() ->
    ?FORALL(
        UUID,
        binary(16),
        UUID =:= keysmith:uuid(keysmith:parse(uuid, UUID), binary)
    ).

prop_type_id_random_uuid(doc) ->
    "Any random 128-bit binary should be encodable and parseable as UUID";
prop_type_id_random_uuid(opts) ->
    [{numtests, 100_000}].
prop_type_id_random_uuid() ->
    ?FORALL(
        UUID,
        binary(16),
        begin
            % Here we just check for now that we don't crash the parser
            _ = keysmith:parse(
                type_id, keysmith:type_id(user, keysmith:parse(uuid, UUID))
            ),
            true
        end
    ).

%--- Generators ----------------------------------------------------------------

uuid_v7() ->
    ?LET(
        {TS, RandA, RandB},
        {ts(), bitstring(12), bitstring(62)},
        {7, TS, RandA, RandB}
    ).

ts() -> growing_integer(10_000_000_000_000).

% proper's integers doesn't grow fast enough
growing_integer(Max) ->
    ?SIZED(Size, resize(round(math:pow(Size, 9)), integer(0, Max))).
