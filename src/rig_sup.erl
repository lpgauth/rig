-module(rig_sup).
-include("rig.hrl").

-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).

%% public
-spec start_link() ->
    {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
-spec init([]) ->
    {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.

init([]) ->
    rig_index:init(),

    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(rig_persist),
        ?CHILD(?SERVER)
    ]}}.
