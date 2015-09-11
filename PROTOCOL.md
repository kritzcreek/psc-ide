# Protocol

Encode the following JSON formats into a single line string
and pass them to psc-ide's stdin. You can then read the result
from psc-ide's stdout as a single line.

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
### List/Cwd/Quit
The `list` command returns all loaded modules. The `cwd` command returns the working
directory of the server(should be your project root). `quit` quits the server.
```json
{
  "command": "list|cwd|quit"
}
```

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
