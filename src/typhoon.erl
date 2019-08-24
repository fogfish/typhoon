-module(typhoon).
-compile({parse_transform, category}).

-export([main/1]).
-export([start/0]).
-export([
   suite/1
]).

main(_) ->
   {ok, _} = application:ensure_all_started(typhoon),
   serverless:spawn(fun identity/1).

identity(#{<<"url">> := Url} = Json) ->
   serverless:notice(#{input => Json}),
   % Json.
   try
      X = suite(Url),
      serverless:notice(X),
      X
   catch _:Reason ->
      serverless:error(Reason),
      {ok, #{}}
   end.

start() ->
   applib:boot(?MODULE, code:where_is_file("sys.config")).


%%
%%
suite({uri, _, _} = Uri) ->
   [either ||
      Path  <- download(Uri),
      Suite =< scalar:atom( filename:basename(Path, ".erl") ),
      typhoon_suite:new(Suite, Path),
      typhoon_suite:run(Suite)
   ];

suite(Uri) ->
   suite(uri:new(Uri)).

%%
%%
download({uri, file, _} = Uri) ->
   {ok, scalar:c(uri:path(Uri))};

download({uri, s3, _} = Uri) ->
   [either ||
      S3 <- s3am:fetch(Uri),
      Path =< scalar:c(filename:join(["/tmp" | uri:segments(Uri)])),
      filelib:ensure_dir(Path),
      cats:unit( file:delete(Path) ),
      streamfs:open(Path, [raw, {chunk, 1024}]),
      streamfs:write(_, S3),
      streamfs:close(_),
      cats:unit(Path)
   ].
