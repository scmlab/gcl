# Guacamole


## Installation

Pull it from GitHub and build it with [`stack`](https://docs.haskellstack.org/en/stable/README/#how-to-install)

```
git clone git@github.com:scmlab/gcl.git
stack repl
```

Install this [extension](https://marketplace.visualstudio.com/items?itemName=scmlab.guacamole) for VS Code, and you should see something when you open a `.gcl` file.

## Commands

<kbd>C-c</kbd> stands for "press <kbd>Ctrl</kbd> and <kbd>c</kbd> at the same time"

| Command                                 |            Keymap             |
| :-------------------------------------- | :---------------------------: |
| resolve a Spec                          | <kbd>C-c</kbd> <kbd>C-r</kbd> |
| restart the whole extension             | <kbd>C-x</kbd> <kbd>C-r</kbd> |


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

To avoid having to rebuild the program everytime we make a little change,
execute the program in the REPL with `-d` instead:

```
:main -d
```

This would allow the client to communicate with `gcl` via a TCP port.

After you have made changes to the program, to rebuild and restart it:

1. terminate the old server (by typing `ctrl-c`)
2. reload the REPL with `:r`
3. start the new server with `:main -d` in the REPL

To reconnect the client with the new server, use <kbd>C-x</kbd> <kbd>C-r</kbd> (restart) to reestablish the connection.

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
