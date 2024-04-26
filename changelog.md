# 0.1.2.0 (2024-04-26)

* Add `upsert` function to public interface (see [#21](https://github.com/klapaucius/vector-hashtables/pull/21)).
* Simplify support of 32-bit architectures via `maxBound` (see [#22](https://github.com/klapaucius/vector-hashtables/pull/22)).
* Improve performance via strictness annotations and bang patterns (see [#24](https://github.com/klapaucius/vector-hashtables/pull/24)).
* Suggest using strict boxed vectors to avoid accumulation of thunks (see [#27](https://github.com/klapaucius/vector-hashtables/pull/27)).
* Speed up division by bucket's size (see [#28](https://github.com/klapaucius/vector-hashtables/pull/28)).
* Avoid deprecated `sizeofMutablePrimArray` (see [#29](https://github.com/klapaucius/vector-hashtables/pull/29)).
* Introduce `findEntry_` and avoid examining `MutVar` twice in `at` (see [#32](https://github.com/klapaucius/vector-hashtables/pull/32)).
* Bump `QuickCheck` boundary (see [#33](https://github.com/klapaucius/vector-hashtables/pull/33)).

# 0.1.1.4 (2023-12-13)

* Add `wasm32` support (see [#20](https://github.com/klapaucius/vector-hashtables/pull/20)).

# 0.1.1.3 (2023-04-23)

* cleanup the cabal file
* CI based on Haskell-CI (see [#15](https://github.com/klapaucius/vector-hashtables/pull/15))
* readme: minimal improvement of benchmark data presentation (see [#16](https://github.com/klapaucius/vector-hashtables/pull/16))
* bump hspec to <2.12 (see [#17](https://github.com/klapaucius/vector-hashtables/pull/17))


# 0.1.1.2 (2023-01-31)

- Relax `hspec` boundaries (see [#14](https://github.com/klapaucius/vector-hashtables/pull/14)).
- Set lower bound for `primtive` (see [#12](https://github.com/klapaucius/vector-hashtables/pull/12)).

# 0.1.1.1 (2021-09-10)

- Optimise `insertWithIndex` function ([#10](https://github.com/klapaucius/vector-hashtables/pull/10)).

# 0.1.1.0 (2021-09-10)

- Add `alter` function to public interface ([#9](https://github.com/klapaucius/vector-hashtables/pull/9)).

# 0.1.0.1 (2021-09-10)

- Remove outdated executable in favor of benchmark.

# 0.1.0.0 (2021-09-07)

- Release vector-hastables to the world.
