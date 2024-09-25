-module(prop_keysmith).
-include_lib("proper/include/proper.hrl").

-compile(nowarn_export_all).
-compile(export_all).

prop_uuid_v7(doc) -> "UUID v7 should be encodable and parseable";
prop_uuid_v7(opts) -> [{numtests, 100_000}].
prop_uuid_v7() ->
    ?FORALL(
        UUID, uuid_v7(), UUID =:= keysmith:parse(uuid, keysmith:uuid(UUID))
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
                {user, UUID} =:= keysmith:parse(type_id, TypeID)
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

%--- Generators ----------------------------------------------------------------

uuid_v7() ->
    ?LET(
        {TS, RandA, RandB},
        {ts(), bitstring(12), bitstring(62)},
        {v7, TS, RandA, RandB}
    ).

ts() -> growing_integer(10_000_000_000_000).

% proper's integers doesn't grow fast enough
growing_integer(Max) ->
    ?SIZED(Size, resize(round(math:pow(Size, 9)), integer(0, Max))).
