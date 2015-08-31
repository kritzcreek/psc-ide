psc-ide
===

Aims to provide editor support for the PureScript language.

Looks to support type lookup, function lookup, autocomplete, etc.

## Supported Commands

### Loading extern.purs files:

 ```
 < psc-ide externFile.purs
 < load
 > Insert the filepath to the extern file to import
 < ./externs.purs
 ```

### Type lookup for functions in the loaded modules:

 ```
 < psc-ide externFile.purs
 < typeLookup
 > Insert the function name to look for:
 < id
 > forall a. a -> a
 ```

### Completion across the loaded modules:

 ```
 < psc-ide externFile.purs
 < completion
 > Insert the function name to look for:
 < fil
 > ["filter", "filterM"]
 ```

### Printing the loaded modules:

 ```
 < psc-ide externFile.purs
 < print
 > ["Data.Array"]
 ```   

You can find the extern files inside the `output/` folder of your project after
running `pulp build` or `psc-make` respectively.

## Installing and Building

The project is set up to be built using the
[stack](https://github.com/commercialhaskell/stack) tool.

```bash
cd psc-ide
stack setup # This is only required if you haven't installed GHC 7.10.2 before
stack build # add --copy-bins to also copy the compiled binaries to ~/.local/bin/
stack exec -- psc-ide externs.purs externs1.purs ...
```

