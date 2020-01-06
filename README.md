# gcl

To develop `gcl`:

```
stack repl
```

To build `gcl`:

```
stack build
```

To build and install the binary `gcl` somewhere in your `$PATH`:

```
stack install
```

To run the tests:

```
stack test
```

To write tests in `ghci`
(need to run `stack test` at least once before this)

```
stack repl gcl:gcl-test
```
