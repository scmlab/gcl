# Guacamole

## Development 

To invoke the REPL:

```
stack repl
```

To build the program:

```
stack build
```

To build and install the binary `gcl` somewhere in your `$PATH`:

```
stack install
```

## Developing with VS Code

Normally the user would have to have `gcl` installed on their machine:

```
stack install
```

However, we can execute the program in the REPL in dev mode instead:

```
:main -d
```

### Testing 

To build the tests: 

```
stack build --test --no-run-tests
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
