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
   case m_http:once(Mod:Fun()) of
      {ok, Expect} ->
         [#{file => Mod, test => Fun, pass => true, expect => Expect, actual => Expect} | Acc];

      {error, {require, Expect, Actual}} ->
         [#{file => Mod, test => Fun, pass => false, expect => Expect, actual => Actual} | Acc]
   end.

