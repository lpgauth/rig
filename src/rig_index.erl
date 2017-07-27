-module(rig_index).
-include("rig.hrl").

-export([
    add/2,
    init/0
]).

-type index() :: atom().

%% public
-spec add(index(), ets:tid()) ->
    ok.

add(Index, Tid) ->
    foil:insert(?INDEX, Index, Tid),
    foil:load(?INDEX),
    ok.

-spec init() ->
    ok.

init() ->
    foil:new(?INDEX).
