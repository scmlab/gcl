# Guacamole

## Installation

Prebuilt binaries should come with [the VS Code Extension](https://marketplace.visualstudio.com/items?itemName=scmlab.guacamole). There's no need of building this project from source unless you are using platforms other than *Windows*, *macOS*, or *Ubuntu*.

### Build from source


1. If you have [`git`](https://git-scm.com/), please clone this repository from GitHub, and then checkout the latest [release](https://github.com/scmlab/gcl/releases) (e.g., `v0.0.17`): 
```shell
git clone git@github.com:scmlab/gcl.git # clone it from GitHub with git
cd gcl # enter the directory
git checkout v0.0.17 # checkout to the latest version with git 
```

If you don't have `git`, you can download the source directly from the one of the [releases](https://github.com/scmlab/gcl/releases).

2. To build the source, you will need [`stack`](https://docs.haskellstack.org/en/stable/README/#how-to-install). Once you have `stack` installed, simply type:

```
stack install
```

This should install the program `gcl` in your PATH. 

3. Download [VS Code](https://code.visualstudio.com/) and install this [extension](https://marketplace.visualstudio.com/items?itemName=scmlab.guacamole) from the marketplace, and you should see something when you open a file with `.gcl` file extension.

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
