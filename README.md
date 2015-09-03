psc-ide
===

Aims to provide editor support for the PureScript language.

Looks to support type lookup, function lookup, autocomplete, etc.

## Emacs Integration
[@epost](https://github.com/epost) wrote a plugin to integrate psc-ide with Emacs at https://github.com/epost/psc-ide-emacs.


## Supported Commands

After you started the server with `psc-ide-server` you can run the following
commands. Make sure you start by loading the modules before you try to query
them.

psc-ide expects the build externs.purs inside the `output/` folder of your
project after running `pulp build` or `psc-make` respectively.

### Loading Modules (expects `/output` to be your build folder):

 ```
 echo "load Data.Array" | psc-ide
 "Success"
 ```

### Type lookup for functions in the loaded modules:

 ```
 echo "typeLookup id" | psc-ide
 "forall a. a -> a"
 ```

### Completion across the loaded modules:

 ```
 echo "complete fil" | psc-ide
 "filter", "filterM"
 ```

### Printing the loaded modules:

 ```
 echo print | psc-ide
 "Data.Array"
 ```

### Quitting the server:
```
echo quit | psc-ide
```

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

