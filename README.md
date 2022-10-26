My solutions to the programming puzzles in the [Advent of Code
2022](https://adventofcode.com/2022) written in Clojure.

# Build and run

Install the Nix package manager then

```
nix build
```

# Develop

Run all tests:
```
nix develop --command clj -X:test
```

Run linting:

```
nix develop --command clj-kondo --lint .
```

```
nix develop --command clj -M:test:eastwood
```
