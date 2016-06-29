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
   random:seed(os:timestamp()),
   {ok, idle, 
      #{
         sock => knet:socket(Url, [{trace, self()}])
      }
   }.

free(_Reason, #{sock := Sock}) ->
   knet:close(Sock).

%%
idle({request, Urn, Peer, Req}, Pipe, #{sock := _Sock} = State) ->
   lists:foreach(fun(X) -> pipe:b(Pipe, X) end, Req),
   {next_state, wire, State#{urn => Urn, peer => Peer, pipe => Pipe}};

idle(_, _, State) ->
   {next_state, idle, State}.


%%
wire({http, _, {Code, _Text, _Head, _Env}}, _, State) ->
   http(os:timestamp(), Code, State),
   {next_state, wire, State#{recv => q:new()}};

wire({http, _, eof}, _, #{recv := Recv, pipe := Pipe, sock := Sock}) ->
   %% @todo: payload size
   pipe:a(Pipe, erlang:iolist_to_binary(q:list(Recv))),
   {next_state, idle, #{sock => Sock}};

wire({http, _, Pckt}, _, #{recv := Recv} = State) ->
   {next_state, wire, State#{recv => q:enq(Pckt, Recv)}};

wire({trace, T, Msg}, _, State) ->
   trace(T, Msg, State),
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
urn(Base, Urn) ->
   uri:join([uri:schema(Urn), uri:path(Urn)], Base).

%%
%%
http(T, Code, #{urn := Urn})
 when Code >= 200, Code < 300 ->
   aura:send(urn({urn, <<"c">>, <<"2xx">>}, Urn), T, 1);

http(T, Code, #{urn := Urn})
 when Code >= 300, Code < 400 ->
   aura:send(urn({urn, <<"c">>, <<"3xx">>}, Urn), T, 1);

http(T, Code, #{urn := Urn})
 when Code >= 400, Code < 500 ->
   aura:send(urn({urn, <<"c">>, <<"4xx">>}, Urn), T, 1);

http(T, Code, #{urn := Urn})
 when Code >= 500, Code < 600 ->
   aura:send(urn({urn, <<"c">>, <<"5xx">>}, Urn), T, 1).

%%
%%   
trace(T, {tcp, connect, X}, #{urn := Urn}) ->
   aura:send(urn({urn, <<"g">>, <<"tcp">>}, Urn), T, X);

trace(T, {tcp, packet, X}, #{urn := Urn}) ->
   aura:send(urn({urn, <<"c">>, <<"pack">>}, Urn), T, 1),
   aura:send(urn({urn, <<"g">>, <<"pack">>}, Urn), T, X);

trace(T, {ssl, handshake, X}, #{urn := Urn}) ->
   aura:send(urn({urn, <<"g">>, <<"ssl">>}, Urn), T, X);

trace(T, {ssl, packet, X}, #{urn := Urn}) ->
   aura:send(urn({urn, <<"c">>, <<"pack">>}, Urn), T, 1),
   aura:send(urn({urn, <<"g">>, <<"pack">>}, Urn), T, X);

trace(_T, {ssl, ca, _X}, #{urn := _Urn}) ->
   ok;

trace(_T, {ssl, peer, _X}, #{urn := _Urn}) ->
   ok;

trace(T, {http, ttfb, X}, #{urn := Urn}) ->
   aura:send(urn({urn, <<"g">>, <<"ttfb">>}, Urn), T, X);

trace(T, {http, ttmr, X}, #{urn := Urn}) ->
   aura:send(urn({urn, <<"g">>, <<"ttmr">>}, Urn), T, X).
