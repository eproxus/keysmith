# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased](https://github.com/eproxus/keysmith/compare/v0.5.0...HEAD)

### Added

- Support for `nil` and `max` UUIDs from RFC 9562
- Support for parsing any UUID variant and version

### Changed

- **BREAKING CHANGE:** Parsed UUIDs are now returned as maps instead of
  shorthand tuples.

## [0.5.0](https://github.com/eproxus/keysmith/releases/tag/v0.5.0) - 2024-09-25

Initial public version of the library.
