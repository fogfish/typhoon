%% @doc
%%   a test suite
-module(typhoon_suite).
-compile({parse_transform, category}).

-export([
   new/2,
   run/1
]).

-type suite()  :: atom().
-type file()   :: string().
-type ast()    :: _.
-type script() :: binary().

%%
%%
-spec new(suite(), file()) -> datum:either(suite()).

new(Suite, File) ->
   [either ||
      compile(Suite, File),
      link(Suite, _)
   ].


%%
-spec compile(suite(), file()) -> datum:either(script()).

compile(Suite, File) ->
   [either ||
      compile_ast(File),
      cats:unit( compile_mod(Suite, _) ),
      compile_obj(_)
   ].

%%
-spec compile_ast(file()) -> datum:either(ast()).

compile_ast(Filename) ->
   case 
      compile:file(
         Filename, 
         ['P', return_errors, binary, {parse_transform, category}, no_error_module_mismatch, {outdir, "/tmp/"}]
      )
   of
      {ok, _, Forms} ->
         {ok, Forms};
      Error ->
         Error
   end.


%%
-spec compile_mod(suite(), ast()) -> datum:either(ast()).

compile_mod(Id, [{attribute, Ln, module, _}|Tail]) ->
   [{attribute, Ln, module, Id} | compile_mod(Id, Tail)];
compile_mod(Id, [Head|Tail]) ->
   [Head | compile_mod(Id, Tail)];
compile_mod(_, []) ->
   [].


%%
-spec compile_obj(ast()) -> datum:either(script()).

compile_obj(Code) ->
   case
      compile:forms(Code, 
         [return_errors, binary, {parse_transform, category}]
      )
   of
      {ok, _, Binary} ->
         {ok, Binary};
      Error ->
         Error
   end.

%%
%%
-spec link(suite(), script()) -> datum:either(suite()).

link(Suite, Code) ->
   code:purge(Suite),
   case code:load_binary(Suite, undefined, Code) of
      {module, Id} ->
         {ok, Suite};
      Error ->
         Error
   end.


%%
%%
run(Suite) ->
   [typhoon_unit:run(Suite, Unit) || 
      {Unit, 0} <- Suite:module_info(exports),
      Unit =/= module_info
   ].



