 -module(reddit_server).

%% Avoid global export_all; export only public functions below.

%% Server that hosts the Gleam engine (calls into compiled reddit_engine module)
%% and simple Erlang clients to demonstrate sync request/response.

 -export([start/0, spawn_engine/0, spawn_clients/1, start_and_spawn/1]).

start() ->
    Pid = spawn_link(fun spawn_engine/0),
    register(reddit_engine_server, Pid),
    io:format("Started reddit_engine_server ~p~n", [Pid]),
    ok.

spawn_engine() ->
    %% Initialize persistence and attempt to load saved state.
    %% If none exists, use a fresh initial state.
    reddit_persistence:start(),
    State0 = case reddit_persistence:load_state() of
        {ok, S} -> S;
        {error, _} -> {engine_state, gleam@list:new(), gleam@list:new(), gleam@list:new(), 0, 0, 0}
    end,
    engine_loop(State0).

engine_loop(State) ->
    receive
            {From, Ref, Msg} ->
                %% Msg expected to be an Erlang tuple matching reddit_engine:handle_message
                try
                    Result = reddit_engine:handle_message(State, Msg),
                    case Result of
                        {NewState, Reply} ->
                            %% Forward the structured reply from the Gleam layer directly
                            From ! {Ref, Reply},
                            engine_loop(NewState);
                        _ ->
                            %% Unexpected reply shape
                            From ! {Ref, {error, unexpected_engine_reply}},
                            engine_loop(State)
                    end
                catch
                    Class:Reason ->
                        From ! {Ref, {error, {Class, Reason}}},
                        engine_loop(State)
                end;
        stop ->
            io:format("Engine stopping - saving state~n"),
            %% Save the last known state before exiting. We assume the state is in the
            %% loop's current binding; for graceful stop we expect the caller to send
            %% stop after any state changes have been processed.
            reddit_persistence:save_state(State),
            ok;
        _Other ->
            %% ignore unknown messages
            engine_loop(State)
    end.

%% Spawn N simple clients that perform synchronous Register -> Join -> CreatePost flows
spawn_clients(N) when is_integer(N), N > 0 ->
    spawn_clients(1, N).

spawn_clients(I, N) when I =< N ->
    Name = lists:concat(["user_", integer_to_list(I)]),
    spawn_link(fun() -> client_process(Name) end),
    spawn_clients(I + 1, N);
spawn_clients(_, _) -> ok.

client_process(Name) ->
    Engine = whereis(reddit_engine_server),
    NameBin = list_to_binary(Name),
    Ref1 = make_ref(),
    Engine ! {self(), Ref1, {join, NameBin}},
    receive
        {Ref1, ok} -> ok
    after 5000 -> io:format("~s: register timeout~n", [Name])
    end,

    Ref2 = make_ref(),
    Engine ! {self(), Ref2, {create_post, NameBin, list_to_binary("general"), list_to_binary("Hello"), list_to_binary("Hi everyone")}},
    receive
        {Ref2, ok} -> ok
    after 5000 -> io:format("~s: create_post timeout~n", [Name])
    end,

    io:format("Client ~s done\n", [Name]).

%% Convenience: start the engine and spawn N clients (useful for quick runs)
start_and_spawn(N) when is_integer(N), N > 0 ->
    start(),
    spawn_clients(N),
    ok.
