-module(reddit_server).
-export([start/0, stop/0, spawn_engine/0]).

%% Start the Reddit engine server
start() ->
    case whereis(reddit_engine_server) of
        undefined ->
            io:format("Initializing Reddit engine...~n"),
            Pid = spawn_link(fun spawn_engine/0),
            register(reddit_engine_server, Pid),
            io:format("Started reddit_engine_server ~p~n", [Pid]),
            {ok, Pid};
        Pid ->
            io:format("Engine already running: ~p~n", [Pid]),
            {ok, Pid}
    end.

%% Stop the engine server
stop() ->
    case whereis(reddit_engine_server) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop,
            timer:sleep(100),
            ok
    end.

%% Spawn the engine loop
spawn_engine() ->
    % Try to load saved state, otherwise use initial state
    State0 = case reddit_persistence:load_state() of
                 {ok, SavedState} ->
                     io:format("Loaded saved state~n"),
                     SavedState;
                 {error, _} ->
                     io:format("Using initial state~n"),
                     {engine_state, [], [], [], [], 0, 0, 0}
             end,
    engine_loop(State0).

%% Main engine loop - receives messages and calls Gleam engine
engine_loop(State) ->
    receive
        {From, Ref, Msg} ->
            try
                % Call Gleam's handle_message function
                Result = reddit_engine:handle_message(State, Msg),

                case Result of
                    % Gleam returns {engine_result, NewState, Reply}
                    {engine_result, NewState, Reply} ->
                        From ! {Ref, Reply},
                        engine_loop(NewState);

                    % Fallback: sometimes just {NewState, Reply}
                    {NewState, Reply} when is_tuple(NewState) ->
                        From ! {Ref, Reply},
                        engine_loop(NewState);

                    Other ->
                        io:format("Unexpected engine result: ~p~n", [Other]),
                        From ! {Ref, {error, "unexpected_engine_reply"}},
                        engine_loop(State)
                end
            catch
                error:undef:Stack ->
                    io:format("Engine function not found!~n"),
                    io:format("Stack: ~p~n", [Stack]),
                    io:format("Available functions: ~p~n", [reddit_engine:module_info(exports)]),
                    From ! {Ref, {error, "engine_function_not_found"}},
                    engine_loop(State);

                Class:Reason:Stack ->
                    io:format("Engine error: ~p:~p~n", [Class, Reason]),
                    io:format("Stack trace: ~p~n", [Stack]),
                    From ! {Ref, {error, "engine_crash"}},
                    engine_loop(State)
            end;

        stop ->
            io:format("Engine stopping - saving state...~n"),
            reddit_persistence:save_state(State),
            io:format("Engine stopped~n"),
            ok;

        {debug, From} ->
            From ! {state, State},
            engine_loop(State);

        Other ->
            io:format("Engine received unknown message: ~p~n", [Other]),
            engine_loop(State)
    end.