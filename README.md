psc-ide
===

Aims to provide editor support for the PureScript language.

Looks to support type lookup, function lookup, autocomplete, etc.

## Sample interaction:

  ```
  psc-ide externs.purs
  Insert the function name to look for:
  filter
  "forall a. (a -> Prim.Boolean) -> Prim.Array a -> Prim.Array a"
  ```

where externs.purs is the Data.Array module externs file.

## Installing and Building

The project is set up to be built using the [stack]() tool.

Usage:
```bash
cd psc-ide
stack setup # This is only required if you haven't installed GHC 7.10.2 before
stack build # add --copy-bins to also copy the compiled binaries to ~/.local/bin/
stack exec -- psc-ide externs.purs externs1.purs ...
```

