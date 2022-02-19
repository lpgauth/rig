-module(rig_index).

-include("rig.hrl").

-export([add/2, init/0]).

-type index() :: atom().

%% public
-spec add(index(), ets:tid()) -> ok.
add(Index, Tid) ->
    ets:insert(?ETS_TABLE_INDEX, {Index, Tid}),
    ok.

-spec init() -> ok.
init() ->
    ets:new(?ETS_TABLE_INDEX, [named_table, public]),
    ok.
