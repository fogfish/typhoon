-module(typhoon_chronolog).
-behavior(pipe).
-compile({parse_transform, category}).

-export([
   start_link/2,
   init/1,
   free/2,
   handle/3
]).

%%
-record(state, {
   suite = undefined,
   unit  = undefined
}).

%%-----------------------------------------------------------------------------
%%
%% factory
%%
%%-----------------------------------------------------------------------------

start_link(Suite, Unit) ->
   pipe:start_link(?MODULE, [Suite, Unit], []).

init([Suite, Unit]) ->
   {ok, handle, 
      #state{
         suite = Suite,
         unit  = Unit
      }
   }.

free(_, _) ->
   ok.

%%-----------------------------------------------------------------------------
%%
%% state machine
%%
%%-----------------------------------------------------------------------------

handle({trace, _Sock, Event}, _, #state{suite = Suite, unit = Unit} = State) ->
   trace(Suite, Unit, Event),
   {next_state, handle, State}.

%%
% disable temporary
% trace(#{key := packet, peer := Peer, t := T, value := Length}) ->
%    send({packet, uri:s(Peer)}, {T, Length});

trace(Suite, Unit, #{key := connect, peer := Peer, t := T, value := Latency}) -> 
   route({Suite, Unit, uri:s(Peer), connect}, {T, Latency});

trace(Suite, Unit, #{key := handshake, peer := Peer, t := T, value := Latency}) -> 
   route({Suite, Unit, uri:s(Peer), handshake}, {T, Latency});

trace(Suite, Unit, #{key := {ttfb, Uri}, t := T, value := Latency}) ->
   route({Suite, Unit, uri:s(Uri), ttfb}, {T, Latency});

trace(Suite, Unit, #{key := {ttmr, Uri}, t := T, value := Latency}) ->
   route({Suite, Unit, uri:s(Uri), ttmr}, {T, Latency});

trace(_, _, _) ->
   undefined.

%%
route(Ticker, Value) ->
   pts:put_(chronolog, Ticker, Value, false).
