%%
%% @doc
%%   example scenario for unit tests
-module(unittest).
-compile({parse_transform, monad}).
 
-export([title/0, t/0, n/0, run/1]).

title() -> 
   "test".

t() ->
   60000.

n() ->
   2.

run(_Config) ->
   do([m_http ||
      _ /= new("http://example.com"),
      _ /= x('GET'),
      _ /= h("Connection", "close"),
      _ /= r(),
      return(_)
   ]).


