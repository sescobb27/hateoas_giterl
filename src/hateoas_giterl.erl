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
    % Get Repo and it's Owner
    [Owner | [Repo| _]] = Args,
    % Fetch Repo's Github Issues
    Body = getIssues(Owner, Repo),
    % Body a plain JSON string so decode it into erlang maps
    Json = decode(Body),
    % From erlang maps get each issue url as an array
    Urls = getIssuesUrl(Json),
    % From the urls array fetch each issue
    IssuesMap = getEachIssue(Urls),
    % Print each issue information
    printIssues(IssuesMap),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
getRepo(Owner, Repo) ->
  % https://api.github.com/repos/${OWNER}/${REPO}
  lists:concat([?GIT_URL, Owner, "/", Repo]).

getPulls(Owner, Repo) ->
  % https://api.github.com/repos/${OWNER}/${REPO}/pulls
  Url = lists:concat([getRepo(Owner, Repo), "/pulls"]),
  % HTTP GET request to the given URL and (empty) Headers
  get(Url, []).

getIssues(Owner, Repo) ->
  % https://api.github.com/repos/${OWNER}/${REPO}/issues
  Url = lists:concat([getRepo(Owner, Repo), "/issues"]),
  % HTTP GET request to the given URL and (empty) Headers
  get(Url, []).

get(Link, Headers) ->
  % Default Headers required to make a Request to Github API
  BaseHeaders = [
    {"accept", "application/vnd.github.v3+json"},
    {"user-agent", "curl/7.43.0"},
    {"authorization", lists:concat(["token ", os:getenv("GIT_OAUTH_TOKEN")])}
  ],
  % Merge Default headers with given Headers but prefer the default ones
  FinalHeaders = lists:merge(BaseHeaders, Headers),
  % Make HTTP Request and only continue if success, then return it's Reponse Body
  {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Link, FinalHeaders}, [], []),
  Body.

decode(Body) ->
  % Use jsone to decode HTTP Response Body
  jsone:decode(binary:list_to_bin(Body), [{object_format, map}]).

getAsync () ->
  % This is a different proccess receiving messages to act upon
  % Messages are in the form of {UniqueId, Client, {URL, Headers}}
  % When URL is a binary convert it to string.
  % then make a HTTP GET request to the given URL and given Headers
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
  % Get url attribute from an array of maps containing Issues atrributes
  lists:map(fun (Issue) ->
    maps:get(<<"url">>, Issue)
  end, Issues).

getEachIssue (Urls) ->
  Client = self(),
  Refs = lists:map(fun(Url) ->
    % Create N short lived processes (N = Urls length)
    % Then call them with a UniqueId for reference, itself to get a response
    % and the URL with Headers
    % {UniqueId, self, {URL, Headers}}
    Pid = spawn(?MODULE, getAsync, []),
    Ref = make_ref(),
    Pid ! {Ref, Client, {Url, []}},
    Ref
  end, Urls),
  % Then for each UniqueId expect a response from the short lived process
  % with the HTTP Response Body
  % return an array of decoded responses
  lists:map(fun (Ref) ->
    receive
      {Ref, Body} ->
        decode(Body)
    end
  end, Refs).

printIssues (Issues) ->
  % Print [STATE][TITLE]: BODY of each issue
  lists:foreach(fun (Issue) ->
    #{<<"title">> := Title, <<"state">> := State, <<"body">> := Body} = Issue,
    io:format("======================================================================================================~n", []),
    io:format("[~s][~s]:~n ~s~n", [State, Title, Body]),
    io:format("======================================================================================================~n", [])
  end, Issues).
