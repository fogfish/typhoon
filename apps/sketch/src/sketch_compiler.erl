%%
-module(sketch_compiler).
-compile({parse_transform, category}).

-export([compile/2]).

compile(Name, Sketch) ->
   [either ||
      compile_ast(Sketch),
      cats:unit( compile_mod(Name, _) ),
      compile_obj(_)
   ].


compile_ast(Filename) ->
   case 
      compile:file(
         Filename, 
         ['P', return_errors, binary, {parse_transform, category}, no_error_module_mismatch]
      )
   of
      {ok, _, Forms} ->
         {ok, Forms};
      Error ->
         Error
   end.


compile_mod(Id, [{attribute, Ln, module, _}|Tail]) ->
   [{attribute, Ln, module, Id} | compile_mod(Id, Tail)];
compile_mod(Id, [Head|Tail]) ->
   [Head | compile_mod(Id, Tail)];
compile_mod(_, []) ->
   [].


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

