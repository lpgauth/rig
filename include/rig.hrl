%% macros
-define(APP, rig).
-define(CHILD(Mod), {Mod, {Mod, start_link, []}, permanent, 5000, worker, [Mod]}).
-define(FILE_READ_SIZE, 65536).
-define(GET_ENV(Key, Default), application:get_env(?APP, Key, Default)).
-define(LOOKUP(Key, List, Default), rig_utils:lookup(Key, List, Default)).
-define(SERVER, rig_server).

%% ETS tables
-define(ETS_TABLE_INDEX, rig_index).

%% msgs
-define(MSG_RELOAD, reload).
-define(MSG_RELOAD_CONFIG, reload_config).

%% defaults
-define(DEFAULT_CLEANUP_DELAY, 500).
-define(DEFAULT_CONFIGS, []).
-define(DEFAULT_KEY_ELEMENT, 1).
-define(DEFAULT_RELOAD_DELAY, 5000).
-define(DEFAULT_SUBSCRIBERS, []).

%% types
-type basedir() :: string().
-type config()  :: {table(), file(), decoder(), options()}.
-type decoder() :: fun((binary()) -> tuple()) | term | {module(), function()}.
-type file()    :: string().
-type key()     :: term().
-type option()  :: {key_element, pos_integer()} | {subscribers, [pid()]}.
-type options() :: [option()].
-type table()   :: atom().
-type value()   :: term().

%% compatibility
-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACK(Stacktrace), Stacktrace).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_), erlang:get_stacktrace()).
-endif.
