%% @doc
%%   network i/o state machine
-module(typhoon_net).
-behaviour(pipe).

-export([
   start_link/2,
   init/1,
   free/2,
   idle/3,
   wire/3
]).

%%
%% 
start_link(_Pq, Url) ->
   pipe:start_link(?MODULE, [Url], []).

init([Url]) ->
   {ok, idle, 
      #{
         % open data socket with latency tracing on
         sock => knet:socket(Url, [{trace, self()}]),
         % open telemetry socket
         aura => aura:socket()
      }
   }.

free(_Reason, #{sock := Sock} = State) ->
   knet:close(Sock).

%%
idle({request, Urn, Peer, Req}, Pipe, #{sock := Sock} = State) ->
   lists:foreach(fun(X) -> pipe:b(Pipe, X) end, Req),
   {next_state, wire, State#{urn => Urn, peer => Peer, pipe => Pipe}};

idle(_, _, State) ->
   {next_state, idle, State}.


%%
wire({http, _, {Code, _Text, Head, _Env}}, _, State) ->
   telemetry(os:timestamp(), {http, status, Code}, State),
   {next_state, wire, State#{recv => q:new()}};

wire({http, _, eof}, _, #{recv := Recv, pipe := Pipe, sock := Sock, aura := Aura} = State) ->
   pipe:a(Pipe, erlang:iolist_to_binary(q:list(Recv))),
   {next_state, idle, #{sock => Sock, aura => Aura}};

wire({http, _, Pckt}, _, #{recv := Recv} = State) ->
   {next_state, wire, State#{recv => q:enq(Pckt, Recv)}};

wire({trace, T, Msg}, _, State) ->
   telemetry(T, Msg, State),
   {next_state, wire, State#{}};

wire(_, _, State) ->
   {next_state, wire, State}.


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% 
telemetry(T, Value, #{aura := Sock, urn := Urn, peer := []}) ->
   ok;
telemetry(T, Value, #{aura := Sock, urn := Urn, peer := Peers}) ->
   aura:send(Sock, Peers, {Urn, T, Value}).

