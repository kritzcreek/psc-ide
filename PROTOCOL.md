# Protocol

Encode the following JSON formats into a single line string and pass them to
psc-ide's stdin. You can then read the result from psc-ide's stdout as a single
line. The result needs to be unwrapped from the "wrapper" which separates success
from failure. This wrapper is described at the end of this document.

## Command:
### Load
The `load` command "loads" the requested modules into the server
for completion and type info.

**Params:**
 - `modules :: (optional) [ModuleName]`: A list of modules to load.
  psc-ide-server will try to parse all the declarations in these modules
 - `dependencies :: (optional) [ModuleName]`: A list of modules to load 
  including their dependencies. In contrast to the `module` field, all the
  imports in these Modules will also be loaded.

```json
{
  "command": "load",
  "params": {
    "modules": (optional)["Module.Name1", "Module.Name2"],
    "dependencies": (optional)["Module.Name3"]
  }
}
```

**Result:**

The Load Command returns a string.

### Type
The `type` command looks up the type for a given identifier.

**Params:**
 - `search :: String`: The identifier to look for. Only matches on equality.
 - `filters :: [Filter]`: These filters will be applied before looking for the
  identifier. These filters get combined with *AND*, so a candidate must match *ALL*
  of them to be eligible.
```json
{
  "command": "type",
  "params": {
    "search": "filterM",
    "filters": [Filter]
  }
}
```

**Result:**
The possible types are returned in the same format as completions

### Complete
The `complete` command looks up possible completions/corrections.

**Params**:
 - `filters :: [Filter]`: The same as for the `type` command. A candidate must match
  all filters.
 - `matcher :: (optional) Matcher`: The strategy used for matching candidates after filtering.
  Results are scored internally and will be returned in the descending order where
  the nth element is better then the n+1-th.

  If no matcher is given every candidate, that passes the filters, is returned in no 
  particular order.
```json
{
  "command": "complete",
  "params": {
    "filters": [Filter],
    "matcher": (optional) Matcher
  }
}
```

**Result:**

The following format is returned as the Result:

```json
[
  {
  "module": "Module1.Name",
  "identifier": "filter",
  "type": "forall a. (a -> Boolean) -> [a] -> [a]"
  }
]
```

### List/Cwd/Quit
`list` returns all loaded modules.

`cwd` returns the working directory of the server(should be your project root).

`quit` quits the server.

```json
{
  "command": "list|cwd|quit"
}
```

**Result:**
These commands return strings.

## Filter:
```json
{
  "filter": "exact",
  "params": {
    "search": "filterM"
  }
}

{
   "filter": "prefix",
   "params": {
     "search": "filt"
   }
}

{
   "filter": "modules",
   "params": {
     "modules": ["My.Module"]
   }
}

{
  "filter": "dependencies",
  "params": {
    "modules": ["My.Module"]
  }
}
```

## Matcher:
```json

{
  "matcher": "flex",
  "params": {
    "search": "filt"
  }
}

{
  "matcher": "distance",
  "params": {
    "search": "dilterM",
    "maxDist": 3
  }
}
```

## Responses

All Responses are wrapped in the following format:

```json
{
  "resultType": "success|error",
  "result": Result|Error
}
```

### Error

Errors at this point are merely Error strings. Newlines are escaped like `\n`
and should be taken care of by the editor-plugin.
