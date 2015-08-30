psc-ide
===

Aims to provide editor support for the PureScript language.

Looks to support type lookup, function lookup, autocomplete, etc.

## Sample interaction:

  ```
  psc-ide externs.purs
  Insert the function name to look for:
  filter
  Right (Just "forall a. (a -> Prim.Boolean) -> Prim.Array a -> Prim.Array a"
  ```

where externs.purs is the Data.Array module externs file.

