-module(sketch_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([
   all/0,
   compile/1,
   load/1
]).

all() ->
   [
      compile,
      load
   ].


compile(Config) ->
   Sketch = filename:join([?config(data_dir, Config), "unittest.erl"]),
   {ok, _} = sketch:compile(unittest, Sketch).

load(Config) ->
   Sketch = filename:join([?config(data_dir, Config), "unittest.erl"]),
   {ok, Code} = sketch:compile(unittest, Sketch),
   {ok, unittest} = sketch:load(unittest, Code).


