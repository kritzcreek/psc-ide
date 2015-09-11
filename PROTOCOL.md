# Protocol

Encode the following JSON formats into a single line string
and pass them to psc-ide's stdin. You can then read the result
from psc-ide's stdout as a single line.

## Command:
```json
{
  "command": "load",
  "params": {
    "modules": (optional)["Module.Name1", "Module.Name2"],
    "dependencies": (optional)["Module.Name3"]
  }
}

{
  "command": "type",
  "params": {
    "search": "filterM",
    "filters": [Filter]
  }
}

{
  "command": "complete",
  "params": {
    "filters": [Filter],
    "matcher": (optional) Matcher
  }
}

{
  "command": "list|cwd|quit"
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
