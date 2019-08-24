-module(typhoon_stats).
-behavior(pipe).
-include_lib("datum/include/datum.hrl").
-compile({parse_transform, category}).

-export([
   start_link/0,
   init/1,
   free/2,
   handle/3
]).

%%
-record(state, {
   sensors   = undefined
}).

start_link() ->
   pipe:start_link(?MODULE, [], []).

init([]) ->
   {ok, handle, 
      #state{
         sensors = gb_sets:new()
      }
   }.

free(_, _) ->
   ok.

handle({trace, _Sock, Event}, _, State) ->
   [identity ||
      trace(Event),
      insert(_, State),
      cats:unit({next_state, handle, _})
   ];

handle(sensors, Pipe, #state{sensors = Sensors} = State) ->
   pipe:ack(Pipe, {ok, gb_sets:to_list(Sensors)}),
   {next_state, handle, State}.

%%
% disable temporary
% trace(#{key := packet, peer := Peer, t := T, value := Length}) ->
%    send({packet, uri:s(Peer)}, {T, Length});

trace(#{key := connect, peer := Peer, t := T, value := Latency}) -> 
   send({connect, uri:s(Peer)}, {T, Latency});

trace(#{key := handshake, peer := Peer, t := T, value := Latency}) -> 
   send({handshake, uri:s(Peer)}, {T, Latency});

trace(#{key := {ttfb, Uri}, t := T, value := Latency}) ->
   send({ttfb, uri:s(Uri)}, {T, Latency});

trace(#{key := {ttmr, Uri}, t := T, value := Latency}) ->
   send({ttmr, uri:s(Uri)}, {T, Latency});

trace(_) ->
   ?None.

%%
insert(?None, State) ->
   State;
insert(Sensor, #state{sensors = Sensors} = State) ->
   State#state{sensors = gb_sets:add(Sensor, Sensors)}.

%%
send(Sensor, Value) ->
   pts:put_(sensor, Sensor, Value, false),
   Sensor.
