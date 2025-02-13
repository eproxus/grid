# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

**Full changelog**: https://github.com/eproxus/grid/compare/v0.2.1...HEAD

### Added

- Official support for Erlang 26 and 27

### Breaking

- Official support for Erlang 24 has been dropped

## 0.2.1 - 2022-12-07

### Changed

- `titlecase` is now uses proper title capitalization (APA style)

**Full changelog**: https://github.com/eproxus/grid/compare/v0.2.0..v0.2.1

## 0.2.0 - 2022-12-05

### Added

- Columns can be aligned by a column spec, e.g. `#{key => foo, alight => right}`
- It is now possible to specify the same column multiple times
- Support mixed column types (index or spec)
- Column contents can now be formatted
- A row can be any term, which counts as a single column row with that term
- Proplists are formatted as maps with proper column keys

**Full changelog**: https://github.com/eproxus/grid/compare/v0.1.0..v0.2.0

## 0.1.0 - 2022-02-28

Initial version.

**Full changelog**: https://github.com/eproxus/grid/releases/tag/v0.1.0
