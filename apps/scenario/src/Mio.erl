%% @doc
%%   used only for scenario compatibility with 0.7.x release
-module('Mio').

-export([return/1, fail/1, '>>='/2]).

-type state(A)  :: fun((_) -> [A|_]).
-type f(A, B)   :: fun((A) -> state(B)).

%%
%%
-spec return(A) -> state(A).

return(X) ->
   fun(State) -> [X|State] end.

%%
%%
-spec fail(_) -> _.

fail(X) ->
   exit(X).


%%
%%
-spec '>>='(state(A), f(A, B)) -> state(B).

'>>='(X, Fun) ->
   join(fmap(Fun, X)).

%%
%%
-spec join(state(state(A))) -> state(A).

join(IO) ->
   fun(State) -> 
      [Fun|Y] = IO(State),
      Fun(Y)
   end.

%%
%%
-spec fmap(fun((A) -> B), state(A)) -> state(B).

fmap(Fun, IO) ->
   fun(State) ->
      case IO(State) of
         [A|Y] ->
            [Fun(A)|Y];
         A ->
            [Fun(A)|State]
      end
   end.
