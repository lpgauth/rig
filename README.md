# rig

Auto-reloading Erlang config indexer

[![Build Status](https://travis-ci.org/lpgauth/rig.svg?branch=dev)](https://travis-ci.org/lpgauth/rig.svg?branch=dev)
[![Coverage Status](https://coveralls.io/repos/github/lpgauth/rig/badge.svg?branch=dev)](https://coveralls.io/github/lpgauth/rig?branch=dev)

## API

```erlang
rig:all(table()) ->
    {ok, [{key(), value()}]} | {error, unknown_table} .

rig:read(table(), key()) ->
    {ok, value()} | {error, unknown_key | unknown_table}.

rig:read(table(), key(), value()) ->
    {ok, value()} | {error, unknown_table}.

rig:version(table()) ->
    {ok, ets:tid()} | {error, unknown_table}.
```

The config files are length delimited using [base 128 varints][1] to encode length.

[1]: https://developers.google.com/protocol-buffers/docs/encoding#varints

## Environment

| Name          | Type                                  | Default | Description                                |
|---------------|---------------------------------------|---------|--------------------------------------------|
| config        | [config() \| {basedir(), [config()]}] | []      | configs to be indexed                      |
| reload_delay  | non_neg_integer()                     | 5000    | time between state reloads in milliseconds |

### Example

```erlang
[{rig, [
  {configs, [
    {applists, "./priv/proto/development/applists.proto", "my_proto:decoder/1.", []},
    {clients, "./priv/bert2/development/clients.bert2", term, []},

    {"./priv/bert2/production/", [
      {ads, "ads.bert2", term, []},
      {sites, "sites.bert2", term, []}
    ]},

    {"./priv/proto/production/", [
      {applists, "applists.proto", "my_proto:decoder/1.", []},
      {sitelists, "sitelists.proto", "my_proto:decoder/1.", []}
    ]},
  ]},
  {reload_delay, 500}
}].
```
## Types

```erlang
-type config()         :: {table_name(), file:filename(), decoder(), config_options()}.
-type config_option()  :: {element_index, pos_integer()} | {subscribers, [pid()]}.
-type config_options() :: [config_option()].
-type decoder()        :: fun((binary()) -> tuple()) | term.
-type table_name()     :: atom().
```

## Tests

```makefile
make dialyzer
make elvis
make eunit
make xref
```

## License

```license
The MIT License (MIT)

Copyright (c) 2016-2017 Louis-Philippe Gauthier

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
