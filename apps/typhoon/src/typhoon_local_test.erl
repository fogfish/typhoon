-module(typhoon_local_test).
-compile({parse_transform, category}).

-export([run/1]).

run(Dir) ->
   fs:fold(fun test/2, [], Dir).

test(Sketch, Acc) ->
   Mod   = scalar:atom( filename:basename(Sketch, ".erl") ),
   Tests = test(Sketch),
   [ #{file => Mod, tests => Tests} | Acc].

test(Sketch) ->
   case 
      [either ||
         Mod =< scalar:atom( filename:basename(Sketch, ".erl") ),
         sketch:compile(Mod, Sketch),
         sketch:load(Mod, _),
         eval(Mod)
      ]
   of
      {ok, Result} ->
         Result;

      {error, Reason} ->
         Reason
   end.

eval(Mod) ->
   {ok, lists:foldl(fun(Fun, Acc) -> eval(Mod, Fun, Acc) end, [], sketch:tests(Mod))}.

eval(Mod, Fun, Acc) ->
   {ok, Pid} = typhoon_stats:start_link(),
   case m_http:eval(10, Mod:Fun(), [{tracelog, Pid}]) of
      {ok, Expect} ->
         [#{file => Mod, test => Fun, pass => true, expect => Expect, actual => Expect, latency => latency(Pid)} | Acc];

      {error, {require, Expect, Actual}} ->
         [#{file => Mod, test => Fun, pass => false, expect => Expect, actual => Actual, latency => latency(Pid)} | Acc]
   end.

latency(Pid) ->
   case 
      [either ||
         Sensors <- pipe:call(Pid, sensors),
         cats:sequence([pts:remove(sensor, X) || X <- Sensors]),
         cats:unit(lists:zip(Sensors, _)),
         cats:unit(lists:foldl(fun append/2, #{}, _))
      ]
   of
      {ok, Latency} ->
         Latency;
      _ ->
         undefined
   end.

append({{Type, Peer}, List}, Json) ->
   lens:put(
      lens:c(lens:at(Peer, #{}), lens:at(Type)),
      percentile(List),
      Json
   ).

percentile(List) ->
   lists:sort([tempus:u(X) || {_, X} <- List]).

