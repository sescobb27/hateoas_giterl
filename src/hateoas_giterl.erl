-module(hateoas_giterl).

%% API exports
-export([main/1,
          getPulls/2,
          getIssues/2,
          get/2,
          getAsync/0,
          getIssuesUrl/1,
          printIssues/1,
          getEachIssue/1]).
-define (GIT_URL, "https://api.github.com/repos/").

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    inets:start(),
    ssl:start(),
    [Owner | [Repo| _]] = Args,
    Body = getIssues(Owner, Repo),
    Json = decode(Body),
    Urls = getIssuesUrl(Json),
    IssuesMap = getEachIssue(Urls),
    printIssues(IssuesMap),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
getRepo(Owner, Repo) ->
  lists:concat([?GIT_URL, Owner, "/", Repo]).

getPulls(Owner, Repo) ->
  Url = lists:concat([getRepo(Owner, Repo), "/pulls"]),
  get(Url, []).

getIssues(Owner, Repo) ->
  Url = lists:concat([getRepo(Owner, Repo), "/issues"]),
  get(Url, []).

get(Link, Headers) ->
  BaseHeaders = [
    {"accept", "application/vnd.github.v3+json"},
    {"user-agent", "curl/7.43.0"},
    {"authorization", lists:concat(["token ", os:getenv("GIT_OAUTH_TOKEN")])}
  ],
  FinalHeaders = lists:merge(BaseHeaders, Headers),
  {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Link, FinalHeaders}, [], []),
  Body.

decode(Body) ->
  jsone:decode(binary:list_to_bin(Body), [{object_format, map}]).

getAsync () ->
  receive
    {Ref, From, {Url, Headers}} when is_binary(Url)->
      Url2 = binary:bin_to_list(Url),
      Body = get(Url2, Headers),
      From ! {Ref, Body};
    {Ref, From, {Url, Headers}} ->
      Body = get(Url, Headers),
      From ! {Ref, Body}
  end.

getIssuesUrl (Issues) ->
  lists:map(fun (Issue) ->
    maps:get(<<"url">>, Issue)
  end, Issues).

getEachIssue (Urls) ->
  Client = self(),
  Refs = lists:map(fun(Url) ->
    Pid = spawn(?MODULE, getAsync, []),
    Ref = make_ref(),
    Pid ! {Ref, Client, {Url, []}},
    Ref
  end, Urls),
  lists:map(fun (Ref) ->
    receive
      {Ref, Body} ->
        decode(Body)
    end
  end, Refs).

printIssues (Issues) ->
  lists:foreach(fun (Issue) ->
    #{<<"title">> := Title, <<"state">> := State, <<"body">> := Body} = Issue,
    io:format("======================================================================================================~n", []),
    io:format("[~s][~s]:~n ~s~n", [State, Title, Body]),
    io:format("======================================================================================================~n", [])
  end, Issues).
