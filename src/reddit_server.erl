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
                     % Create proper initial Gleam state structure
                     {engine_state, [], [], [], [], 0, 0, 0}
             end,
    engine_loop(State0).

%% Main engine loop - receives messages and calls Gleam engine
engine_loop(State) ->
    receive
        {From, Ref, Msg} ->
            try
                % Convert the message to proper Gleam format
                GleamMsg = convert_to_gleam_msg(Msg),

                % Call Gleam's handle_message function
                Result = reddit_engine:handle_message(State, GleamMsg),

                % Handle the response
                case Result of
                    % Gleam returns {engine_result, NewState, Reply}
                    {engine_result, NewState, Reply} ->
                        ErlangReply = convert_from_gleam_reply(Reply),
                        From ! {Ref, ErlangReply},
                        engine_loop(NewState);

                    % Fallback for direct tuple format
                    {NewState, Reply} when is_tuple(NewState) ->
                        ErlangReply = convert_from_gleam_reply(Reply),
                        From ! {Ref, ErlangReply},
                        engine_loop(NewState);

                    Other ->
                        io:format("Unexpected engine result: ~p~n", [Other]),
                        From ! {Ref, {error, "unexpected_engine_reply"}},
                        engine_loop(State)
                end
            catch
                error:undef:Stack ->
                    io:format("Engine function not found!~n"),
                    io:format("Message received: ~p~n", [Msg]),
                    io:format("Stack: ~p~n", [Stack]),
                    io:format("Available functions: ~p~n", [reddit_engine:module_info(exports)]),
                    From ! {Ref, {error, "engine_function_not_found"}},
                    engine_loop(State);

                error:{badmatch, _} = Reason:Stack ->
                    io:format("Pattern match error in engine: ~p~n", [Reason]),
                    io:format("Message: ~p~n", [Msg]),
                    io:format("Stack: ~p~n", [Stack]),
                    From ! {Ref, {error, "pattern_match_failed"}},
                    engine_loop(State);

                error:function_clause:Stack ->
                    io:format("Function clause error - message format may be wrong~n"),
                    io:format("Message: ~p~n", [Msg]),
                    io:format("Stack: ~p~n", [Stack]),
                    From ! {Ref, {error, "invalid_message_format"}},
                    engine_loop(State);

                Class:Reason:Stack ->
                    io:format("Engine error: ~p:~p~n", [Class, Reason]),
                    io:format("Message: ~p~n", [Msg]),
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

%% ============================================================================
%% Convert Erlang message tuples to Gleam variant format
%% ============================================================================

convert_to_gleam_msg({register, Username, Password, PublicKey}) ->
    % Gleam: Register(name: String, password: String, public_key: String)
    {register, ensure_binary(Username), ensure_binary(Password), ensure_binary(PublicKey)};

convert_to_gleam_msg({register, Username, Password}) ->
    % Fallback for old format - add empty public key
    {register, ensure_binary(Username), ensure_binary(Password), <<"">>};

convert_to_gleam_msg({login, Username, Password}) ->
    % Gleam: Login(name: String, password: String)
    {login, ensure_binary(Username), ensure_binary(Password)};

convert_to_gleam_msg({join_sub, User, Subreddit}) ->
    % Gleam: JoinSub(user: String, subreddit: String)
    {join_sub, ensure_binary(User), ensure_binary(Subreddit)};

convert_to_gleam_msg({leave_sub, User, Subreddit}) ->
    % Gleam: LeaveSub(user: String, subreddit: String)
    {leave_sub, ensure_binary(User), ensure_binary(Subreddit)};

convert_to_gleam_msg({create_post, Author, Subreddit, Title, Body, Signature}) ->
    % Gleam: CreatePost(author, subreddit, title, body, signature)
    {create_post,
        ensure_binary(Author),
        ensure_binary(Subreddit),
        ensure_binary(Title),
        ensure_binary(Body),
        ensure_binary(Signature)};

convert_to_gleam_msg({create_post, Author, Subreddit, Title, Body}) ->
    % Fallback for old format without signature
    {create_post,
        ensure_binary(Author),
        ensure_binary(Subreddit),
        ensure_binary(Title),
        ensure_binary(Body),
        <<"unsigned">>};

convert_to_gleam_msg({vote, Voter, PostId, Delta}) ->
    % Gleam: Vote(voter, post_id, delta)
    {vote, ensure_binary(Voter), ensure_integer(PostId), ensure_integer(Delta)};

convert_to_gleam_msg({create_comment, Author, PostId, ParentId, Body}) ->
    % Gleam: CreateComment(author, post_id, parent_comment_id, body)
    {create_comment,
        ensure_binary(Author),
        ensure_integer(PostId),
        ensure_integer(ParentId),
        ensure_binary(Body)};

convert_to_gleam_msg({get_feed, User, Page, PageSize}) ->
    % Gleam: GetFeed(user, page, page_size)
    {get_feed, ensure_binary(User), ensure_integer(Page), ensure_integer(PageSize)};

convert_to_gleam_msg({get_post, PostId}) ->
    % Gleam: GetPost(post_id)
    {get_post, ensure_integer(PostId)};

convert_to_gleam_msg({send_direct_message, From, To, Body}) ->
    % Gleam: SendDirectMessage(from, to, body)
    {send_direct_message, ensure_binary(From), ensure_binary(To), ensure_binary(Body)};

convert_to_gleam_msg({get_direct_messages, User}) ->
    % Gleam: GetDirectMessages(user)
    {get_direct_messages, ensure_binary(User)};

convert_to_gleam_msg({get_public_key, Username}) ->
    % Gleam: GetPublicKey(username)
    {get_public_key, ensure_binary(Username)};

convert_to_gleam_msg(Other) ->
    io:format("Warning: Unknown message format: ~p~n", [Other]),
    Other.

%% ============================================================================
%% Convert Gleam reply variants to Erlang-friendly format
%% ============================================================================

convert_from_gleam_reply({ok}) -> ok;
convert_from_gleam_reply(ok) -> ok;
convert_from_gleam_reply({ok_with_id, Id}) -> {ok_with_id, Id};
convert_from_gleam_reply({error, Msg}) -> {error, ensure_string(Msg)};
convert_from_gleam_reply({posts, Posts}) -> {posts, Posts};
convert_from_gleam_reply({posts_page, Posts, Page, PageSize, Total}) ->
    {posts_page, Posts, Page, PageSize, Total};
convert_from_gleam_reply({post_data, Post}) -> {post_data, Post};
convert_from_gleam_reply({user_data, User}) -> {user_data, User};
convert_from_gleam_reply({direct_messages, Messages}) -> {direct_messages, Messages};
convert_from_gleam_reply({public_key_data, PublicKey}) -> {public_key_data, PublicKey};
convert_from_gleam_reply(Other) -> Other.

%% Helper to ensure data is a binary (Gleam string)
ensure_binary(Data) when is_binary(Data) -> Data;
ensure_binary(Data) when is_list(Data) ->
    % Check if it's already a string
    case io_lib:printable_unicode_list(Data) of
        true -> list_to_binary(Data);
        false -> list_to_binary(lists:flatten(io_lib:format("~p", [Data])))
    end;
ensure_binary(Data) when is_atom(Data) ->
    atom_to_binary(Data, utf8);
ensure_binary(Data) ->
    list_to_binary(lists:flatten(io_lib:format("~p", [Data]))).

%% Helper to ensure data is a string (for error messages)
ensure_string(Data) when is_binary(Data) -> binary_to_list(Data);
ensure_string(Data) when is_list(Data) -> Data;
ensure_string(Data) when is_atom(Data) -> atom_to_list(Data);
ensure_string(Data) -> lists:flatten(io_lib:format("~p", [Data])).

%% Helper to ensure data is an integer
ensure_integer(Data) when is_integer(Data) -> Data;
ensure_integer(Data) when is_list(Data) -> list_to_integer(Data);
ensure_integer(Data) when is_binary(Data) -> binary_to_integer(Data);
ensure_integer(_) -> 0.