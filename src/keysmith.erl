-module(keysmith).
-moduledoc """
Library for generating unique IDs.

KeySmith is a library for generating unique IDs. It supports two types of
IDs:

* UUID ([RFC-9562](https://www.rfc-editor.org/rfc/rfc9562.html))
    * Version 4
    * Version 7
* TypeID ([specification](https://github.com/jetify-com/typeid/tree/main/spec))
    * TypeID's embed a UUID v7 by default, but can be customized with any
      supported UUID version.
""".

-ignore_xref(uuid/1).
-export([uuid/1]).
-ignore_xref(uuid/2).
-export([uuid/2]).
-ignore_xref(type_id/1).
-export([type_id/1]).
-ignore_xref(type_id/2).
-export([type_id/2]).
-ignore_xref(parse/2).
-export([parse/2]).

% Disable unused macro warning since it has false positives
-hank([unused_macros]).

-ifdef(TEST).
-ignore_xref(cb32_encode/1).
-export([cb32_encode/1]).
-ignore_xref(cb32_decode/1).
-export([cb32_decode/1]).
-endif.

%--- Types ---------------------------------------------------------------------

-doc "An atom representing the type of ID".
-type id() :: uuid | type_id.
-export_type([id/0]).

-doc """
UNIX timestamp in millisecond resolution.

Timestamp value as generated from
[`erlang:system_time(millisecond)`](https://www.erlang.org/doc/apps/erts/erlang.html#system_time/1).
""".
-type uuid_v7_unix_ts_ms() :: non_neg_integer().
-export_type([uuid_v7_unix_ts_ms/0]).

-doc "A 48-bit random bitstring used in UUID generation".
-type uuid_rand_a() :: <<_:48>>.
-export_type([uuid_rand_a/0]).

-doc "A 12-bit random bitstring used in UUID generation".
-type uuid_rand_b() :: <<_:12>>.
-export_type([uuid_rand_b/0]).

-doc "A 62-bit random bitstring used in UUID generation".
-type uuid_rand_c() :: <<_:62>>.
-export_type([uuid_rand_c/0]).

-doc "UUID version 4 (with optional random components)".
-type uuid_v4() ::
    v4
    | {v4, uuid_rand_a(), uuid_rand_b(), uuid_rand_c()}.
-export_type([uuid_v4/0]).

-doc "UUID version 7 (with optional timestamp and random components)".
-type uuid_v7() ::
    v7
    | {v7, uuid_v7_unix_ts_ms()}
    | {v7, uuid_v7_unix_ts_ms(), uuid_rand_b(), uuid_rand_c()}.
-export_type([uuid_v7/0]).

-doc "An Erlang representation of a UUID".
-type uuid_spec() :: uuid_v4() | uuid_v7().
-export_type([uuid_spec/0]).

-doc "UUID output format".
-type uuid_format() :: binary | bytes | hex | hex_nodash.
-export_type([uuid_format/0]).

-doc "UUID in raw binary format".
-type uuid_binary() :: <<_:128>>.
-export_type([uuid_binary/0]).

-doc "UUID in hex format with dashes".
-type uuid_hex() :: <<_:288>>.
-export_type([uuid_hex/0]).

-doc "UUID in hex format without dashes".
-type uuid_hex_nodash() :: <<_:256>>.
-export_type([uuid_hex_nodash/0]).

-doc "A generated UUID value".
-type uuid() :: uuid_binary() | uuid_hex() | uuid_hex_nodash().
-export_type([uuid/0]).

-doc "An Erlang representation of a TypeID".
-type type_id_spec() :: {type_id_prefix(), uuid_spec()}.
-export_type([type_id_spec/0]).

-doc "A TypeID 'type' prefix".
-type type_id_prefix() :: atom().
-export_type([type_id_prefix/0]).

-doc "A generated TypeID".
-type type_id() :: binary().
-export_type([type_id/0]).

%--- Macros --------------------------------------------------------------------

-define(UUID_V4_VER, 4).
-define(UUID_V7_VER, 7).
-define(UUID_VAR, 2#10).

% erlfmt-ignore
-define(UUID_HEX(A, B, C, D, E), <<
    (A):8/bytes, $-,
    (B):4/bytes, $-,
    (C):4/bytes, $-,
    (D):4/bytes, $-,
    (E):12/bytes
>>).
-define(UUID_HEX_NODASH(A, B, C, D, E),
    <<A:8/bytes, B:4/bytes, C:4/bytes, D:4/bytes, E:12/bytes>>
).
-define(UUID_V7_TO_BINARY(TS, RandB, RandC),
    <<(TS):48, ?UUID_V7_VER:4, RandB/bits, ?UUID_VAR:2, RandC/bits>>
).
-define(BINARY_TO_UUID_V7(TS, RandB, RandC),
    <<(TS):48, ?UUID_V7_VER:4, RandB:12/bits, ?UUID_VAR:2, RandC:62/bits>>
).
-define(UUID_V4_TO_BINARY(RandA, RandB, RandC),
    <<RandA/bits, ?UUID_V4_VER:4, RandB/bits, ?UUID_VAR:2, RandC/bits>>
).
-define(BINARY_TO_UUID_V4(RandA, RandB, RandC),
    <<RandA:48/bits, ?UUID_V4_VER:4, RandB:12/bits, ?UUID_VAR:2, RandC:62/bits>>
).

%--- API -----------------------------------------------------------------------

-doc """
Create a TypeID for the specified type prefix.

This creates a TypeID with the specified type prefix and a UUID version 7 suffix
including the current Unix system time in milliseconds.

## Example

```erlang
1> keysmith:type_id(my_type).
<<"my_type_01j8j55ywkesavxf5avszv4jv1">>
```

Equivalent to [`type_id(Type, v7)`](#type-id/2).
""".
-spec type_id(Type :: type_id_prefix()) -> TypeID :: type_id().
type_id(Type) -> type_id(Type, v7).

-doc """
Generate a type ID for the specified type and UUID format. The UUID format can
be any format supported by [`uuid/2`](#uuid/2).

TypeIDs are encoded with a lowercase version of Crockford's base32 encoding.

## Example

```erlang
1> keysmith:type_id(my_type, {v4, <<0:48>>, <<0:12>>, <<0:62>>}).
<<"my_type_00000000008008000000000000">>
```
""".
-spec type_id(Type :: type_id_prefix(), UUID :: uuid_spec()) ->
    TypeID :: type_id().
type_id(Type, UUID) ->
    <<
        (type_tag(Type))/bytes,
        $_,
        (cb32_encode(<<0:2, (uuid(UUID, binary))/bytes>>))/bytes
    >>.

-doc #{equiv => uuid(Type, hex)}.
-spec uuid(Version :: uuid_spec()) -> UUID :: uuid().
uuid(Version) -> uuid(Version, hex).

-doc """
Generate a UUID in the specified format.

The following types are supported:
* `v4` - Random UUID v4.
* `{v4, RandA, RandB, RandC}` - UUID v4 with set random parts.
* `v7` - UUID v7 with the current system timestamp and a random component.
* `{v7, TS}` - UUID v7 with a set timestamp and a random component.
* `{v7, TS, RandB, RandC}` - UUID v7 with a set timestamp and set random components.

The following formats are supported:
* `bytes` / `binary` - UUID as a raw 16 byte (128 bit) binary.
* `hex` - UUID as a hexadecimal lowercase string with dashes.
* `hex_nodash` - UUID as a hexadecimal lowercise string without dashes.

If custom random parts are provided, they must be bitstrings of exactly 48, 12,
and 62 bits respectively (for example assuming each variable contains an
integer, the binary syntax would be `<<RandA:48>>`, `<<RandB:12>>`, and
`<<RandC:62>>`).

## Errors

This fuction raises the following exceptions:

* `error:{invalid_uuid_spec, Spec}` when an unsupported or invalid specification
  is specified.
* `error:{invalid_uuid_format, Format}` when an unsupported or invalid format is
  specified.

## Example

```erlang
1> keysmith:uuid(v4).
<<"38f18026-c2ee-4b5b-a8f7-439c33fb2a7a">>
2> keysmith:uuid(v7).
<<"019228d7-b720-7484-ab83-25332e9b0a33">>
```
""".
-spec uuid(Type :: uuid_spec(), Format :: uuid_format()) -> UUID :: uuid().
uuid(v7, Format) ->
    uuid({v7, erlang:system_time(millisecond)}, Format);
uuid({v7, TS}, Format) ->
    <<RandB:12/bits, RandC:62/bits, _/bits>> =
        crypto:strong_rand_bytes(10),
    format(?UUID_V7_TO_BINARY(TS, RandB, RandC), Format);
uuid({v7, TS, RandB, RandC}, Format) when
    bit_size(RandB) =:= 12, bit_size(RandC) =:= 62
->
    format(?UUID_V7_TO_BINARY(TS, RandB, RandC), Format);
uuid(v4, Format) ->
    <<RandA:48/bits, RandB:12/bits, RandC:62/bits, _/bits>> =
        crypto:strong_rand_bytes(16),
    format(?UUID_V4_TO_BINARY(RandA, RandB, RandC), Format);
uuid({v4, RandA, RandB, RandC}, Format) ->
    format(?UUID_V4_TO_BINARY(RandA, RandB, RandC), Format);
uuid(Spec, _Format) ->
    error({invalid_uuid_spec, Spec}).

-doc """
Parse an ID into its components.

Format can be one of `uuid` or `type_id` ([`id_type()`](#id_type/0)).

* `uuid`

  UUID will get decoded automatically based on their encoded type. The supported
  types are listed in the [`uuid/2`](#uuid/2) documentation. Only the full
  format of the UUID tuples can be returned by this function.

  ### Example

  ```erlang
  1> UUIDv7 = keysmith:uuid({v7, 123}).
  <<"00000000-007b-7054-9789-64298c4136ee">>
  2> keysmith:parse(uuid, UUIDv7).
  {v7,123,<<5,4:4>>,<<94,37,144,166,49,4,219,46:6>>}
  ```

* `type_id`

  A TypeID will get decoded to a tuple of the type and the UUID format. The type
  **must** exist as atom for this fuction to return succesfully (it uses
  [`binary_to_existing_atom/1`](https://www.erlang.org/doc/apps/erts/erlang.html#binary_to_existing_atom/1)
  internally).

  The UUID suffix format must be in lowercase Crockfords's base32 encoding,
  otherwise an error will be thrown.

  ### Example

  ```erlang
  1> TypeID = keysmith:type_id(my_type).
  <<"my_type_01j8m75gy0fqws0860vmrqv1rr">>
  2> keysmith:parse(type_id, TypeID).
  {my_type,{v7,1727255462848,
               <<223,9:4>>,
               <<64,131,3,116,197,246,28,24:6>>}}
  ```

## Errors

This fuction raises the following exceptions:

* `{invalid_id, type_id, ID}` if a TypeID is parsed and is not a valid TypeID:
    * The type is not decodable into an existing atom
    * The UUID suffix not being a 26-byte lowercase Crockfords's base32
      string
* `{invalid_id, uuid, ID}` if a UUID is parsed and the ID is not a valid UUID,
  or if a TypeID contains a UUID suffix that is not a valid UUID:
    * The UUID not being a valid hexadecimal 36-byte UUID (with dashes)
    * The UUID not being a valid hexadecimal 32-byte UUID (without dashes)
    * The UUID not being a raw binary UUID of 128 bits
* `{invalid_id_type, Type}` if the format is not supported.
""".
-spec parse
    (Type :: uuid, UUID :: uuid()) -> uuid_spec();
    (Type :: type_id, TypeID :: type_id()) -> type_id_spec().
parse(type_id, TypeID) ->
    case string:split(TypeID, <<$_>>, trailing) of
        [<<>>, _ID] ->
            error({invalid_id, type_id, TypeID});
        [Tag, ID] when byte_size(ID) == 26 ->
            {AtomTag, UUID} =
                try
                    case cb32_decode(ID) of
                        <<0:2, UUIDBin:128/bits>> ->
                            {binary_to_existing_atom(Tag), UUIDBin};
                        _Other ->
                            error({invalid_id, type_id, TypeID})
                    end
                catch
                    error:badarg -> error({invalid_id, type_id, TypeID})
                end,
            {AtomTag, parse(uuid, UUID)};
        _ ->
            error({invalid_id, type_id, TypeID})
    end;
parse(uuid, ?BINARY_TO_UUID_V7(TS, RandB, RandC)) ->
    {v7, TS, RandB, RandC};
parse(uuid, ?BINARY_TO_UUID_V4(RandA, RandB, RandC)) ->
    {v4, RandA, RandB, RandC};
parse(uuid, ?UUID_HEX(A, B, C, D, E)) ->
    parse(uuid, binary:decode_hex(?UUID_HEX_NODASH(A, B, C, D, E)));
parse(uuid, Value) ->
    error({invalid_id, uuid, Value});
parse(Type, _Value) ->
    error({invalid_id_type, Type}).

%--- Internal ------------------------------------------------------------------

% TODO: Support uppercase hex
format(<<A:4/bytes, B:2/bytes, C:2/bytes, D:2/bytes, E:6/bytes>>, hex) ->
    ?UUID_HEX(
        binary:encode_hex(A, lowercase),
        binary:encode_hex(B, lowercase),
        binary:encode_hex(C, lowercase),
        binary:encode_hex(D, lowercase),
        binary:encode_hex(E, lowercase)
    );
format(UUID, binary) ->
    UUID;
% FIXME: Remove 'bin' alias. Add 'bytes' instead?
format(UUID, bytes) ->
    UUID;
format(UUID, hex_nodash) ->
    binary:encode_hex(UUID, lowercase);
format(_UUID, Format) ->
    error({invalid_uuid_format, Format}).

type_tag(Type) when is_atom(Type) -> atom_to_binary(Type).

% erlfmt-ignore
cb32_encode(<<
    B1:5, B2:5, B3:5, B4:5, B5:5, B6:5, B7:5, B8:5, B9:5, B10:5, B11:5, B12:5,
    B13:5, B14:5, B15:5, B16:5, B17:5, B18:5, B19:5, B20:5, B21:5, B22:5, B23:5,
    B24:5, B25:5, B26:5
>>) ->
    <<
        (cb32e(B1)), (cb32e(B2)), (cb32e(B3)), (cb32e(B4)), (cb32e(B5)),
        (cb32e(B6)), (cb32e(B7)), (cb32e(B8)), (cb32e(B9)), (cb32e(B10)),
        (cb32e(B11)), (cb32e(B12)), (cb32e(B13)), (cb32e(B14)), (cb32e(B15)),
        (cb32e(B16)), (cb32e(B17)), (cb32e(B18)), (cb32e(B19)), (cb32e(B20)),
        (cb32e(B21)), (cb32e(B22)), (cb32e(B23)), (cb32e(B24)), (cb32e(B25)),
        (cb32e(B26))
    >>.

% erlfmt-ignore
cb32e(N) ->
    element(N + 1, {
        $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a, $b, $c, $d, $e, $f, $g, $h,
        $j, $k, $m, $n, $p, $q, $r, $s, $t, $v, $w, $x, $y, $z
    }).

% erlfmt-ignore
cb32_decode(<<
    B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17,
    B18, B19, B20, B21, B22, B23, B24, B25, B26
>>) ->
    <<
        (cb32ds(B1)):5, (cb32ds(B2)):5, (cb32ds(B3)):5, (cb32ds(B4)):5,
        (cb32ds(B5)):5, (cb32ds(B6)):5, (cb32ds(B7)):5, (cb32ds(B8)):5,
        (cb32ds(B9)):5, (cb32ds(B10)):5, (cb32ds(B11)):5, (cb32ds(B12)):5,
        (cb32ds(B13)):5, (cb32ds(B14)):5, (cb32ds(B15)):5, (cb32ds(B16)):5,
        (cb32ds(B17)):5, (cb32ds(B18)):5, (cb32ds(B19)):5, (cb32ds(B20)):5,
        (cb32ds(B21)):5, (cb32ds(B22)):5, (cb32ds(B23)):5, (cb32ds(B24)):5,
        (cb32ds(B25)):5, (cb32ds(B26)):5
    >>.

% erlfmt-ignore
cb32ds(N) ->
    % $0 is 48, so we subtract 47 to avoid specifying invalid values
    element(N - 47, {
        % Range: 0-9
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
        % :;<=>?@ (invalid)
        bad, bad, bad, bad, bad, bad, bad,
        % A-Z (invalid)
        bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
        bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad, bad,
        % [\]^_` (invalid)
        bad, bad, bad, bad, bad, bad,
        % a-h
        10, 11, 12, 13, 14, 15, 16, 17,
        % i (maps to 1)
        bad,
        % j-k
        18, 19,
        % l (maps to 1)
        bad,
        % m-n
        20, 21,
        % o (maps to 0)
        bad,
        % p-t
        22, 23, 24, 25, 26,
        % u (maps to v)
        bad,
        % v-z
        27, 28, 29, 30, 31
    }).

% Tolerant decode (unused for now)
% erlfmt-ignore
% cb32dt(N) ->
%     % $0 is 48, so we subtract 47 to avoid specifying invalid values
%     element(N - 47, {
%         % Range: 0-9
%         0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
%         % :;<=>?@ (invalid)
%         bad, bad, bad, bad, bad, bad, bad,
%         % A-H
%         10, 11, 12, 13, 14, 15, 16, 17,
%         % I (maps to 1)
%         1,
%         % J-K
%         18, 19,
%         % L (maps to 1)
%         1,
%         % M-N
%         20, 21,
%         % O (maps to 0)
%         0,
%         % P-T
%         22, 23, 24, 25, 26,
%         % U (maps to V)
%         27,
%         % V-Z
%         27, 28, 29, 30, 31,
%         % [\]^_` (invalid)
%         bad, bad, bad, bad, bad, bad,
%         % a-h
%         10, 11, 12, 13, 14, 15, 16, 17,
%         % i (maps to 1)
%         1,
%         % j-k
%         18, 19,
%         % l (maps to 1)
%         1,
%         % m-n
%         20, 21,
%         % o (maps to 0)
%         0,
%         % p-t
%         22, 23, 24, 25, 26,
%         % u (maps to v)
%         27,
%         % v-z
%         27, 28, 29, 30, 31
%     }).
