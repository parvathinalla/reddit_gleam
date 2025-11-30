-module(reddit_persistence).
-export([start/0, save_state/1, load_state/0, clear/0]).

-define(TABLE, reddit_engine_persist).

%% Initialize ETS table for state persistence
start() ->
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set]),
            ok;
        _ ->
            ok
    end.

%% Save engine state to ETS
save_state(State) ->
    start(),
    ets:insert(?TABLE, {engine_state, State}),
    ok.

%% Load engine state from ETS
load_state() ->
    start(),
    case ets:lookup(?TABLE, engine_state) of
        [{engine_state, State}] -> {ok, State};
        [] -> {error, not_found}
    end.

%% Clear all persisted data
clear() ->
    start(),
    ets:delete_all_objects(?TABLE),
    ok.