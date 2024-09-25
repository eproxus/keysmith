# keysmith [![Erlang CI][ci-img]][ci]

[ci]:     https://github.com/eproxus/keysmith/actions/workflows/erlang.yml
[ci-img]: https://github.com/eproxus/keysmith/actions/workflows/erlang.yml/badge.svg

Keysmith is a well tested and performant library for generating unique
identifiers. Keysmith currently supports the following formats:

* UUID
  * Version 4 (random)
  * Version 7 (time-based)
* TypeID
  * Defaults to UUID version 7
  * Can use any supported UUID version

## Usage

KeySmith provides functions to generate UUIDs and TypeIDs. Here are some basic
examples:

### Generating UUIDs

You can generate UUIDs version 4 (random) and version 7 (time-based):

```erlang
% Generate a UUID v4
1> keysmith:uuid(v4).
<<"79782daf-ff7d-4426-a8f5-e12672e89e32">>

% Generate a UUID v7
2> keysmith:uuid(v7).
<<"01922919-44d3-779c-84ad-20ab20fc7dbb">>

% Generate a UUID v7 with a specific timestamp
3> keysmith:uuid({v7, 1234567890}).
<<"00004996-02d2-7bf9-9b68-599f55d37931">>
```

UUIDs can be generated in different formats:

```erlang
4> keysmith:uuid(v4, binary).
<<193,169,209,27,160,43,73,83,147,64,63,136,127,77,166,39>>

5> keysmith:uuid(v4, hex_nodash).
<<"01922919d63d7de990a0209e23b50747">>
```

### Generating TypeIDs

TypeIDs are a combination of a type prefix and a UUID. They are useful for
creating namespaced unique identifiers:

```erlang
% Generate a TypeID with default UUID v7
6> keysmith:type_id(user).
<<"user_01j8mhm5vafn8vprb7g6dz4q90">>

% Generate a TypeID with a specific UUID
7> keysmith:type_id(product, v4).
<<"product_7fv003jbwg9ptvecwr68p6n5k6">>
```

### Parsing IDs

KeySmith can also parse UUIDs and TypeIDs back into their components:

```erlang
8> UUID = keysmith:uuid(v7).
<<"019228d7-b720-7484-ab83-25332e9b0a33">>
9> keysmith:parse(uuid, UUID).
{v7,1727266452545,
    <<78,9:4>>,
    <<143,127,240,176,213,200,238,33:6>>}

10> TypeID = keysmith:type_id(order).
<<"order_01hm90qbvvgb31sqtrd5zs8cj2">>
11> keysmith:parse(type_id, TypeID).
{order,{v7,1727266470507,
           <<32,11:4>>,
           <<133,246,230,190,48,142,227,36:6>>}}
```

For more detailed information on available functions and options, please refer
to the module documentation.

## Changelog

See the [Releases](https://github.com/eproxus/keysmith/releases) page.

## Code of Conduct

Find this project's code of conduct in
[Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md).

## Contributing

First of all, thank you for contributing with your time and energy.

If you want to request a new feature make sure to
[open a feature request](https://github.com/eproxus/keysmith/issues/new?template=feature_request.yaml)
so we can discuss it first.

Bug reports and questions are also welcome. If you believe you've found a bug,
feel free to [report an issue](https://github.com/eproxus/keysmith/issues/new?template=bug_report.yaml).

If you have a question, search the discussions and issues since it might have
already been answered before. If you can't find an answer, you
can [open a new discussion](https://github.com/eproxus/keysmith/discussions/new/choose).

Contributions will be subject to the MIT License. You will retain the copyright.

For more information check out [CONTRIBUTING.md](CONTRIBUTING.md).

## Security

This project's security policy is made explicit in [SECURITY.md](SECURITY.md).

## Conventions

### Versions

This project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

### License

This project uses the [MIT License](LICENSE.md).
