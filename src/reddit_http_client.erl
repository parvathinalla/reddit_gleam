-module(reddit_http_client).
-export([post/2, get/1]).

post(Url, Body) ->
  application:ensure_all_started(inets),
  UrlStr = ensure_string(Url),
  BodyStr = ensure_string(Body),
  Request = {UrlStr, [], "application/json", BodyStr},
  HTTPOptions = [{timeout, 10000}],
  Options = [{body_format, binary}],

  try
    case httpc:request(post, Request, HTTPOptions, Options) of
      {ok, {{_Version, StatusCode, _}, _Headers, ResponseBody}} when StatusCode >= 200, StatusCode < 300 ->
        ResponseStr = ensure_string(ResponseBody),
        {ok, ResponseStr};
      {ok, {{_Version, StatusCode, _}, _Headers, ResponseBody}} ->
        ResponseStr = ensure_string(ResponseBody),
        ErrorMsg = lists:flatten(io_lib:format("HTTP ~p: ~s", [StatusCode, ResponseStr])),
        {error, ErrorMsg};
      {error, Reason} ->
        ErrorMsg2 = lists:flatten(io_lib:format("Connection failed: ~p", [Reason])),
        {error, ErrorMsg2}
    end
  catch
    Type:Error ->
      ErrorMsg3 = lists:flatten(io_lib:format("Request failed: ~p:~p", [Type, Error])),
      {error, ErrorMsg3}
  end.

get(Url) ->
  application:ensure_all_started(inets),
  UrlStr = ensure_string(Url),
  HTTPOptions = [{timeout, 10000}],
  Options = [{body_format, binary}],

  try
    case httpc:request(get, {UrlStr, []}, HTTPOptions, Options) of
      {ok, {{_Version, StatusCode, _}, _Headers, ResponseBody}} when StatusCode >= 200, StatusCode < 300 ->
        ResponseStr = ensure_string(ResponseBody),
        {ok, ResponseStr};
      {ok, {{_Version, StatusCode, _}, _Headers, ResponseBody}} ->
        ResponseStr = ensure_string(ResponseBody),
        ErrorMsg = lists:flatten(io_lib:format("HTTP ~p: ~s", [StatusCode, ResponseStr])),
        {error, ErrorMsg};
      {error, Reason} ->
        ErrorMsg2 = lists:flatten(io_lib:format("Connection failed: ~p", [Reason])),
        {error, ErrorMsg2}
    end
  catch
    Type:Error ->
      ErrorMsg3 = lists:flatten(io_lib:format("Request failed: ~p:~p", [Type, Error])),
      {error, ErrorMsg3}
  end.

ensure_string(Data) when is_binary(Data) ->
  binary_to_list(Data);
ensure_string(Data) when is_list(Data) ->
  case io_lib:printable_list(Data) of
    true -> Data;
    false -> lists:flatten(io_lib:format("~p", [Data]))
  end;
ensure_string(Data) ->
  lists:flatten(io_lib:format("~p", [Data])).