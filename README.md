# DEPRECATION WARNING: 

psc-ide is now distributed with the compiler. This repo won't be kept in sync.

psc-ide
===

A tool which provides editor support for the PureScript language.

[![Build Status](https://travis-ci.org/kRITZCREEK/psc-ide.svg?branch=travis-build)](https://travis-ci.org/kRITZCREEK/psc-ide)

## Installation

Compiled binaries are provided for Windows, Ubuntu and OSX on the [release page](https://github.com/kRITZCREEK/psc-ide/releases).
It is however very easy to build psc-ide with stack or cabal.

After you installed the psc-ide binaries you can start using one
of the editor plugins.

### Using stack
`stack install psc-ide`

### Using cabal
For building with cabal the use of sandboxes is highly recommended to avoid "cabal hell"
```
mkdir psc-ide
cd psc-ide
cabal update
cabal sandbox init
cabal install psc-ide
```
And then copy the compiled binaries from `.cabal-sandbox/bin/` into a folder on your path.

### From Source
For the most recent version of psc-ide you can compile from master doing:
```
git clone https://github.com/kRITZCREEK/psc-ide.git
cd psc-ide
stack install
```

## Editor Integration
* [@epost](https://github.com/epost) wrote a plugin to integrate psc-ide with Emacs at https://github.com/epost/psc-ide-emacs.
* Atom integration is available with https://github.com/nwolverson/atom-ide-purescript.
* Visual Studio Code integration is available with https://github.com/nwolverson/vscode-ide-purescript.
* Vim integration is available here: https://github.com/FrigoEU/psc-ide-vim.

## Running the Server
Start the server by running the `psc-ide-server` executable.
It supports the following options:

- `-p / --port` specify a port. Defaults to 4242
- `-d / --directory` specify the toplevel directory of your project. Defaults to
  the current directory
- `--output-directory`: Specify where to look for compiled output inside your
  project directory. Defaults to `output/`, relative to either the current
  directory or the directory specified by `-d`.
- `--debug`: Enables some logging meant for debugging
- `--version`: Output psc-ide version

## Issuing queries

After you started the server you can start issuing requests using psc-ide.
Make sure you start by loading the modules before you try to query them.

psc-ide expects the build externs.purs inside the `output/` folder of your
project after running `pulp build` or `psc-make` respectively.

(If you changed the port of the server you can change the port for psc-ide by
using the -p option accordingly)

## Protocol

For a documentation have a look at:
[PROTOCOL.md](PROTOCOL.md)

## Installing and Building

The project is set up to be built using the
[stack](https://github.com/commercialhaskell/stack) tool.

```bash
cd psc-ide
stack setup # This is only required if you haven't installed GHC 7.10.2 before
stack build # add --copy-bins to also copy the compiled binaries to ~/.local/bin/
stack exec -- psc-ide-server &
stack exec -- psc-ide
```

## Testing

The testsuite can be run with `stack test`.
