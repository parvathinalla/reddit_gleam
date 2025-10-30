-module(reddit_persistence).
-compile([export_all]).

%% Simple ETS-backed persistence for engine state.
%% Stores a single key 'engine_state' containing the Erlang term for the engine state.

-define(TABLE, reddit_engine_persist).

start() ->
    %% Create the ETS table if it doesn't already exist.
    case ets:info(?TABLE) of
        undefined ->
            ets:new(?TABLE, [named_table, public, set]);
        _ -> ok
    end.

save_state(State) ->
    start(),
    ets:insert(?TABLE, {engine_state, State}),
    ok.

load_state() ->
    start(),
    case ets:lookup(?TABLE, engine_state) of
        [{engine_state, State}] -> {ok, State};
        [] -> {error, not_found}
    end.

clear() ->
    start(),
    ets:delete_all_objects(?TABLE),
    ok.
