-module(reddit_http_server).
-export([start/0, start/1, stop/0, accept_loop/1, handle_client/1]).

%% ============= SERVER STARTUP =============

start() -> start(8080).

start(Port) ->
  io:format("~n╔════════════════════════════════════════════════════════════╗~n"),
  io:format("║       Reddit Clone REST API Server v2.0                   ║~n"),
  io:format("╚════════════════════════════════════════════════════════════╝~n~n"),

  % Start the engine
  io:format("Starting Reddit engine...~n"),
  case reddit_server:start() of
    {ok, Pid} ->
      io:format("✓ Engine started successfully (PID: ~p)~n~n", [Pid]),
      start_tcp_server(Port);
    Error ->
      io:format("✗ Failed to start engine: ~p~n", [Error]),
      {error, engine_start_failed}
  end.

start_tcp_server(Port) ->
  case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]) of
    {ok, ListenSocket} ->
      io:format("✓ HTTP server started on http://localhost:~p~n~n", [Port]),
      print_endpoints(),
      io:format("~nServer ready! Waiting for client connections...~n~n"),

      spawn(fun() -> accept_loop(ListenSocket) end),
      register(reddit_http_server, self()),

      receive
        stop ->
          gen_tcp:close(ListenSocket),
          reddit_server:stop(),
          ok
      end;
    {error, Reason} ->
      io:format("✗ Failed to start HTTP server: ~p~n", [Reason]),
      {error, Reason}
  end.

print_endpoints() ->
  io:format("Available REST API endpoints:~n"),
  io:format("  POST   /api/register         - Register new user~n"),
  io:format("  POST   /api/login            - Login user~n"),
  io:format("  POST   /api/subreddits/:name/join   - Join subreddit~n"),
  io:format("  POST   /api/subreddits/:name/leave  - Leave subreddit~n"),
  io:format("  POST   /api/posts            - Create new post~n"),
  io:format("  GET    /api/posts/:id        - Get post by ID~n"),
  io:format("  POST   /api/posts/:id/vote   - Vote on post~n"),
  io:format("  POST   /api/posts/:id/comments - Add comment~n"),
  io:format("  GET    /api/feed             - Get user feed~n"),
  io:format("  POST   /api/messages/send    - Send direct message~n"),
  io:format("  GET    /api/messages         - Get user messages~n").

stop() ->
  case whereis(reddit_http_server) of
    undefined -> ok;
    Pid -> Pid ! stop, ok
  end.

%% ============= CONNECTION HANDLING =============

accept_loop(ListenSocket) ->
  case gen_tcp:accept(ListenSocket, 10000) of
    {ok, Socket} ->
      spawn(fun() -> handle_client(Socket) end),
      accept_loop(ListenSocket);
    {error, timeout} ->
      accept_loop(ListenSocket);
    {error, closed} ->
      ok;
    {error, Reason} ->
      io:format("Accept error: ~p~n", [Reason]),
      timer:sleep(1000),
      accept_loop(ListenSocket)
  end.

handle_client(Socket) ->
  case gen_tcp:recv(Socket, 0, 10000) of
    {ok, Data} ->
      try
        Request = parse_http_request(Data),
        Response = handle_http_request(Request),
        gen_tcp:send(Socket, Response)
      catch
        Error:Reason:Stack ->
          io:format("Error handling request: ~p:~p~n~p~n", [Error, Reason, Stack]),
          ErrorResponse = build_response(500, "{\"error\":\"internal_server_error\"}"),
          gen_tcp:send(Socket, ErrorResponse)
      end,
      gen_tcp:close(Socket);
    {error, _Reason} ->
      gen_tcp:close(Socket)
  end.

%% ============= HTTP PARSING =============

parse_http_request(Data) ->
  Lines = binary:split(Data, <<"\r\n">>, [global]),
  case Lines of
    [RequestLine | Rest] ->
      [Method, Path, _Version] = binary:split(RequestLine, <<" ">>, [global]),
      {Headers, BodyLines} = parse_headers(Rest, []),
      Body = case BodyLines of
               [] -> <<>>;
               [B | _] -> B
             end,
      #{
        method => binary_to_list(Method),
        path => binary_to_list(Path),
        headers => Headers,
        body => Body
      };
    _ ->
      #{method => "GET", path => "/", headers => [], body => <<>>}
  end.

parse_headers([], Acc) ->
  {lists:reverse(Acc), []};
parse_headers([<<>> | Rest], Acc) ->
  {lists:reverse(Acc), Rest};
parse_headers([Line | Rest], Acc) ->
  case binary:split(Line, <<": ">>) of
    [Key, Value] ->
      Header = {string:lowercase(binary_to_list(Key)), binary_to_list(Value)},
      parse_headers(Rest, [Header | Acc]);
    _ ->
      parse_headers(Rest, Acc)
  end.

%% ============= REQUEST ROUTING =============

handle_http_request(#{method := Method, path := Path, headers := _Headers, body := Body}) ->
  io:format("→ ~s ~s~n", [Method, Path]),

  {StatusCode, ResponseBody} = try
                                 route_request(Method, Path, Body)
                               catch
                                 _:Error ->
                                   io:format("  ✗ Error: ~p~n", [Error]),
                                   {500, "{\"error\":\"internal_error\"}"}
                               end,

  build_response(StatusCode, ResponseBody).

route_request("GET", "/", _Body) ->
  {200, "{\"status\":\"ok\",\"version\":\"2.0\",\"message\":\"Reddit Clone API v2.0\"}"};

route_request("GET", "/health", _Body) ->
  {200, "{\"status\":\"healthy\",\"engine\":\"running\"}"};

route_request("POST", "/api/register", Body) ->
  handle_register(Body);

route_request("POST", "/api/login", Body) ->
  handle_login(Body);

route_request("POST", "/api/subreddits/" ++ Rest, Body) ->
  case string:split(Rest, "/", all) of
    [Name, "join"] -> handle_join_subreddit(Name, Body);
    [Name, "leave"] -> handle_leave_subreddit(Name, Body);
    _ -> {404, "{\"error\":\"not_found\"}"}
  end;

route_request("POST", "/api/posts", Body) ->
  handle_create_post(Body);

route_request("GET", "/api/posts/" ++ IdStr, _Body) ->
  handle_get_post(IdStr);

route_request("POST", "/api/posts/" ++ Rest, Body) ->
  case string:split(Rest, "/", all) of
    [IdStr, "vote"] -> handle_vote_post(IdStr, Body);
    [IdStr, "comments"] -> handle_create_comment(IdStr, Body);
    _ -> {404, "{\"error\":\"not_found\"}"}
  end;

route_request("GET", "/api/feed", Body) ->
  handle_get_feed(Body);

route_request("GET", "/api/messages", Body) ->
  handle_get_messages(Body);

route_request("POST", "/api/messages/send", Body) ->
  handle_send_message(Body);

route_request(_Method, _Path, _Body) ->
  {404, "{\"error\":\"not_found\"}"}.

%% ============= REQUEST HANDLERS =============
handle_register(Body) ->
  case parse_json(Body) of
    {ok, Json} ->
      Username = proplists:get_value("username", Json, "unknown"),
      Password = proplists:get_value("password", Json, "password"),

      Ref = make_ref(),
      reddit_engine_server ! {self(), Ref, {register, Username, Password, ""}},  % Added "" for public_key

      receive
        {Ref, ok} ->
          io:format("  ✓ Registered: ~s~n", [Username]),
          {201, "{\"status\":\"success\",\"username\":\"" ++ Username ++ "\"}"};
        {Ref, {error, Msg}} ->
          io:format("  ✗ Registration failed: ~s~n", [Msg]),
          {400, "{\"status\":\"error\",\"message\":\"" ++ Msg ++ "\"}"}
      after 5000 ->
        {500, "{\"error\":\"timeout\"}"}
      end;
    {error, _} ->
      {400, "{\"error\":\"invalid_json\"}"}
  end.

handle_login(Body) ->
  case parse_json(Body) of
    {ok, Json} ->
      Username = proplists:get_value("username", Json, "unknown"),
      Password = proplists:get_value("password", Json, "password"),

      Ref = make_ref(),
      reddit_engine_server ! {self(), Ref, {login, Username, Password}},

      receive
        {Ref, ok} ->
          Token = generate_token(Username),
          io:format("  ✓ Logged in: ~s~n", [Username]),
          {200, "{\"status\":\"success\",\"token\":\"" ++ Token ++ "\",\"username\":\"" ++ Username ++ "\"}"};
        {Ref, {error, _}} ->
          io:format("  ✗ Login failed for: ~s~n", [Username]),
          {401, "{\"status\":\"error\",\"message\":\"Invalid credentials\"}"}
      after 5000 ->
        {500, "{\"error\":\"timeout\"}"}
      end;
    {error, _} ->
      {400, "{\"error\":\"invalid_json\"}"}
  end.

handle_join_subreddit(Name, Body) ->
  case parse_json(Body) of
    {ok, Json} ->
      Username = proplists:get_value("username", Json, "unknown"),

      Ref = make_ref(),
      reddit_engine_server ! {self(), Ref, {join_sub, Username, Name}},

      receive
        {Ref, ok} ->
          io:format("  ✓ ~s joined ~s~n", [Username, Name]),
          {200, "{\"status\":\"success\",\"message\":\"Joined " ++ Name ++ "\"}"};
        {Ref, {error, Msg}} ->
          {400, "{\"status\":\"error\",\"message\":\"" ++ Msg ++ "\"}"}
      after 5000 ->
        {500, "{\"error\":\"timeout\"}"}
      end;
    {error, _} ->
      {400, "{\"error\":\"invalid_json\"}"}
  end.

handle_leave_subreddit(Name, Body) ->
  case parse_json(Body) of
    {ok, Json} ->
      Username = proplists:get_value("username", Json, "unknown"),

      Ref = make_ref(),
      reddit_engine_server ! {self(), Ref, {leave_sub, Username, Name}},

      receive
        {Ref, ok} ->
          io:format("  ✓ ~s left ~s~n", [Username, Name]),
          {200, "{\"status\":\"success\",\"message\":\"Left " ++ Name ++ "\"}"};
        {Ref, {error, Msg}} ->
          {400, "{\"status\":\"error\",\"message\":\"" ++ Msg ++ "\"}"}
      after 5000 ->
        {500, "{\"error\":\"timeout\"}"}
      end;
    {error, _} ->
      {400, "{\"error\":\"invalid_json\"}"}
  end.

handle_create_post(Body) ->
  case parse_json(Body) of
    {ok, Json} ->
      Username = proplists:get_value("username", Json, "unknown"),
      Subreddit = proplists:get_value("subreddit", Json, "general"),
      Title = proplists:get_value("title", Json, "Untitled"),
      PostBody = proplists:get_value("body", Json, ""),

      Ref = make_ref(),
      reddit_engine_server ! {self(), Ref, {create_post, Username, Subreddit, Title, PostBody}},

      receive
        {Ref, ok} ->
          io:format("  ✓ Post created by ~s in ~s~n", [Username, Subreddit]),
          {201, "{\"status\":\"success\",\"message\":\"Post created in " ++ Subreddit ++ "\"}"};
        {Ref, {error, Msg}} ->
          {400, "{\"status\":\"error\",\"message\":\"" ++ Msg ++ "\"}"}
      after 5000 ->
        {500, "{\"error\":\"timeout\"}"}
      end;
    {error, _} ->
      {400, "{\"error\":\"invalid_json\"}"}
  end.

handle_get_post(IdStr) ->
  case catch list_to_integer(IdStr) of
    Id when is_integer(Id) ->
      Ref = make_ref(),
      reddit_engine_server ! {self(), Ref, {get_post, Id}},

      receive
        {Ref, {post_data, _Post}} ->
          {200, "{\"post\":{\"id\":" ++ integer_to_list(Id) ++ ",\"message\":\"Post found\"}}"};
        {Ref, {error, Msg}} ->
          {404, "{\"error\":\"" ++ Msg ++ "\"}"}
      after 5000 ->
        {500, "{\"error\":\"timeout\"}"}
      end;
    _ ->
      {400, "{\"error\":\"invalid_post_id\"}"}
  end.

handle_vote_post(IdStr, Body) ->
  case catch list_to_integer(IdStr) of
    Id when is_integer(Id) ->
      case parse_json(Body) of
        {ok, Json} ->
          Username = proplists:get_value("username", Json, "unknown"),
          DeltaStr = proplists:get_value("delta", Json, "0"),
          Delta = case catch list_to_integer(DeltaStr) of
                    D when is_integer(D) -> D;
                    _ -> 0
                  end,

          Ref = make_ref(),
          reddit_engine_server ! {self(), Ref, {vote, Username, Id, Delta}},

          receive
            {Ref, ok} ->
              io:format("  ✓ Vote recorded on post #~p~n", [Id]),
              {200, "{\"status\":\"success\",\"message\":\"Vote recorded\"}"};
            {Ref, {error, Msg}} ->
              {400, "{\"status\":\"error\",\"message\":\"" ++ Msg ++ "\"}"}
          after 5000 ->
            {500, "{\"error\":\"timeout\"}"}
          end;
        {error, _} ->
          {400, "{\"error\":\"invalid_json\"}"}
      end;
    _ ->
      {400, "{\"error\":\"invalid_post_id\"}"}
  end.

handle_create_comment(IdStr, Body) ->
  case catch list_to_integer(IdStr) of
    PostId when is_integer(PostId) ->
      case parse_json(Body) of
        {ok, Json} ->
          Username = proplists:get_value("username", Json, "unknown"),
          CommentBody = proplists:get_value("body", Json, ""),
          ParentIdStr = proplists:get_value("parent_id", Json, "0"),
          ParentId = case catch list_to_integer(ParentIdStr) of
                       P when is_integer(P) -> P;
                       _ -> 0
                     end,

          Ref = make_ref(),
          reddit_engine_server ! {self(), Ref, {create_comment, Username, PostId, ParentId, CommentBody}},

          receive
            {Ref, ok} ->
              io:format("  ✓ Comment added to post #~p~n", [PostId]),
              {201, "{\"status\":\"success\",\"message\":\"Comment added\"}"};
            {Ref, {error, Msg}} ->
              {400, "{\"status\":\"error\",\"message\":\"" ++ Msg ++ "\"}"}
          after 5000 ->
            {500, "{\"error\":\"timeout\"}"}
          end;
        {error, _} ->
          {400, "{\"error\":\"invalid_json\"}"}
      end;
    _ ->
      {400, "{\"error\":\"invalid_post_id\"}"}
  end.

handle_get_feed(Body) ->
  case parse_json(Body) of
    {ok, Json} ->
      Username = proplists:get_value("username", Json, "unknown"),

      Ref = make_ref(),
      reddit_engine_server ! {self(), Ref, {get_feed, Username, 1, 10}},

      receive
        {Ref, {posts_page, _Posts, Page, _PageSize, Total}} ->
          io:format("  ✓ Feed for ~s: ~p posts~n", [Username, Total]),
          {200, "{\"posts\":[],\"page\":" ++ integer_to_list(Page) ++ ",\"total\":" ++ integer_to_list(Total) ++ ",\"message\":\"Feed retrieved\"}"};
        _ ->
          {500, "{\"error\":\"unexpected_response\"}"}
      after 5000 ->
        {500, "{\"error\":\"timeout\"}"}
      end;
    {error, _} ->
      {400, "{\"error\":\"invalid_json\"}"}
  end.

handle_get_messages(Body) ->
  case parse_json(Body) of
    {ok, Json} ->
      Username = proplists:get_value("username", Json, "unknown"),

      Ref = make_ref(),
      reddit_engine_server ! {self(), Ref, {get_direct_messages, Username}},

      receive
        {Ref, {direct_messages, _Messages}} ->
          io:format("  ✓ Messages retrieved for ~s~n", [Username]),
          {200, "{\"messages\":[],\"message\":\"Messages retrieved\"}"};
        _ ->
          {500, "{\"error\":\"unexpected_response\"}"}
      after 5000 ->
        {500, "{\"error\":\"timeout\"}"}
      end;
    {error, _} ->
      {400, "{\"error\":\"invalid_json\"}"}
  end.

handle_send_message(Body) ->
  case parse_json(Body) of
    {ok, Json} ->
      FromUser = proplists:get_value("from", Json, "unknown"),
      ToUser = proplists:get_value("to", Json, "unknown"),
      MessageBody = proplists:get_value("body", Json, ""),

      Ref = make_ref(),
      reddit_engine_server ! {self(), Ref, {send_direct_message, FromUser, ToUser, MessageBody}},

      receive
        {Ref, ok} ->
          io:format("  ✓ Message sent from ~s to ~s~n", [FromUser, ToUser]),
          {201, "{\"status\":\"success\",\"message\":\"Message sent\"}"};
        {Ref, {error, Msg}} ->
          {400, "{\"status\":\"error\",\"message\":\"" ++ Msg ++ "\"}"}
      after 5000 ->
        {500, "{\"error\":\"timeout\"}"}
      end;
    {error, _} ->
      {400, "{\"error\":\"invalid_json\"}"}
  end.

%% ============= HTTP RESPONSE BUILDER =============

build_response(StatusCode, Body) ->
  StatusText = status_text(StatusCode),
  Response = [
    "HTTP/1.1 ", integer_to_list(StatusCode), " ", StatusText, "\r\n",
    "Content-Type: application/json\r\n",
    "Content-Length: ", integer_to_list(length(Body)), "\r\n",
    "Access-Control-Allow-Origin: *\r\n",
    "Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS\r\n",
    "Access-Control-Allow-Headers: Content-Type, Authorization\r\n",
    "Connection: close\r\n",
    "\r\n",
    Body
  ],
  list_to_binary(Response).

status_text(200) -> "OK";
status_text(201) -> "Created";
status_text(400) -> "Bad Request";
status_text(401) -> "Unauthorized";
status_text(404) -> "Not Found";
status_text(500) -> "Internal Server Error";
status_text(_) -> "Unknown".

%% ============= HELPER FUNCTIONS =============

generate_token(Username) ->
  Timestamp = erlang:system_time(millisecond),
  Data = Username ++ ":" ++ integer_to_list(Timestamp),
  base64:encode_to_string(crypto:hash(sha256, Data)).

parse_json(Body) when is_binary(Body) ->
  parse_json(binary_to_list(Body));
parse_json(Body) when is_list(Body) ->
  try
    Clean = string:trim(Body, both, "{}\" \n\r\t"),
    case Clean of
      "" -> {ok, []};
      _ ->
        Pairs = string:split(Clean, ",", all),
        Props = lists:map(fun(Pair) ->
          case string:split(Pair, ":") of
            [Key, Value] ->
              CleanKey = string:trim(Key, both, "\" \n\r\t"),
              CleanValue = string:trim(Value, both, "\" \n\r\t"),
              {CleanKey, CleanValue};
            _ ->
              {"", ""}
          end
                          end, Pairs),
        {ok, Props}
    end
  catch
    _:_ ->
      {error, parse_error}
  end;
parse_json(_) ->
  {error, invalid_input}.