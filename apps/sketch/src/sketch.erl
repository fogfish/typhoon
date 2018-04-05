%% @doc
%%   compiles and executes unit tests (sketches)
-module(sketch).

-export([
   compile/2,
   load/2,
   tests/1,
   htmlify/1
]).

-type unit()   :: atom().
-type sketch() :: string().
-type code()   :: binary().


%%
%% 
-spec compile(unit(), sketch()) -> datum:either(code()).

compile(Mod, Sketch) ->
   sketch_compiler:compile(Mod, Sketch).


%%
%%
-spec load(unit(), code()) -> datum:either(unit()).

load(Mod, Code) ->
   code:purge(Mod),
   case code:load_binary(Mod, undefined, Code) of
      {module, Id} ->
         {ok, Mod};
      Error ->
         Error
   end.


%%
%%
-spec tests(unit()) -> [atom()].

tests(Mod) ->
   [Fun || {Fun, 0} <- Mod:module_info(exports), Fun =/= module_info].


%%
%%
-spec htmlify(_) -> {ok, _}.

htmlify(Json) ->
   File = filename:join([code:priv_dir(sketch), "sketch.html"]),
   {ok, Html} = file:read_file(File),
   Fun0 = swirl:f(Html),
   Fun1 = Fun0(undefined),
   Xxxx = Fun1(json(Json)),
   file:write_file("xxxx.html", Xxxx).

json(Json) ->
   Check = lists:flatmap(fun(#{tests := X}) -> X end, Json),
   {Pass, Fail} = lists:partition(fun(#{pass := X}) -> X end, Check),
   #{size => length(Check), pass => length(Pass), fail => length(Fail), tests => Json}.



