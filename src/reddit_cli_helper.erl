-module(reddit_cli_helper).
-export([build_request/2, extract_body/1]).

%% Build HTTP request tuple for httpc
build_request(Url, Body) ->
  {Url, [], "application/json", Body}.

%% Extract body from HTTP response tuple
extract_body({{_Version, _StatusCode, _ReasonPhrase}, _Headers, Body}) ->
  Body;
extract_body(_) ->
  "error".