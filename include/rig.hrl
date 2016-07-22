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
-type config()  :: {table(), file(), decoder(), options()}.
-type decoder() :: fun((binary()) -> tuple()) | term.
-type file()    :: file:filename().
-type key()     :: term().
-type option()  :: {key_element, pos_integer()} | {subscribers, [pid()]}.
-type options() :: [option()].
-type table()   :: atom().
-type value()   :: term().
