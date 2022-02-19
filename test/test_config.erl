-module(test_config).

-export([dummy/1]).

dummy(_Bin) ->
    {domain, 5, <<"foobar.com">>}.
