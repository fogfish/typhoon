-module(typhoon_unit).
-compile({parse_transform, category}).

-export([
   run/2
]).

run(Suite, Unit) ->
   [either ||
      Logger <- typhoon_chronolog:start_link(Suite, Unit),
      Result <- cats:unit(eval(Suite, Unit, Logger)),
      pipe:free(Logger),
      cats:unit(Result)
   ].

eval(Suite, Unit, Logger) ->
   case m_http:eval(10, Suite:Unit(), #{tracelog => Logger})  of
      {ok, Expect} ->
         #{expect => Expect, actual => Expect, latency => latency(Suite, Unit)};

      {error, {require, Expect, Actual}} ->
         #{expect => Expect, actual => Actual, latency => latency(Suite, Unit)}
   end.

latency(Suite, Unit) ->
   [#{type => Type, url => Url, latency => values(Key)} ||
      {{_, _, Url, Type} = Key, _} <- pns:lookup(chronolog, {Suite, Unit, '_', '_'})
   ].

values(Key) ->
   [tempus:u(X) || {_, X} <- pts:remove(chronolog, Key)].
