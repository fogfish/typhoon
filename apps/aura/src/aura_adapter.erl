%%
%% @doc
%%   adapts knet telemetry to data processing messages
-module(aura_adapter).
-behaviour(pipe).
-compile({parse_transform, category}).

%% @todo
%%   * make permanent naming for adapter component

-export([
   start_link/0
  ,init/1
  ,free/2
  ,handle/3
]).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

start_link() ->
   pipe:start_link(?MODULE, [], []).

init([]) ->
   {ok, handle, #{}}.

free(_Reason, _State) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% pipe
%%%
%%%----------------------------------------------------------------------------   

handle({trace, T, _, _, _, {Event, URI}, Value}, _Pipe, State) ->
   trace(uri:schema(URI), Event, URI, T, Value),
   {next_state, handle, State}.

uid(Type, URI) ->
   uri:s( uri:schema([Type, uri:schema(URI)], URI) ).

trace(tcp, connect, URI, T, X) ->
   aura:send({urn, <<"g">>, uid(connect, URI)}, T, X);

trace(tcp, packet, URI, T, X) ->
   aura:send({urn, <<"c">>, uid(packet, URI)}, T, 1), %% packet per second
   aura:send({urn, <<"g">>, uid(packet, URI)}, T, X); %% packet size 

trace(ssl, connect, URI, T, X) ->
   aura:send({urn, <<"g">>, uid(connect, URI)}, T, X);

trace(ssl, handshake, URI, T, X) ->
   aura:send({urn, <<"g">>, uid(handshake, URI)}, T, X);

trace(ssl, packet, URI, T, X) ->
   aura:send({urn, <<"c">>, uid(packet, URI)}, T, 1),
   aura:send({urn, <<"g">>, uid(packet, URI)}, T, X);

trace(ssl, ca, _, _, _) ->
   ok;
trace(ssl, peer, _, _, _) ->
   ok;

trace(http, code, URI, T, Code)
 when Code >= 200, Code < 300 ->
   aura:send({urn, <<"c">>, uid('2xx', URI)}, T, 1);

trace(http, code, URI, T, Code)
 when Code >= 300, Code < 400 ->
   aura:send({urn, <<"c">>, uid('3xx', URI)}, T, 1);

trace(http, code, URI, T, Code)
 when Code >= 400, Code < 500 ->
   aura:send({urn, <<"c">>, uid('4xx', URI)}, T, 1);

trace(http, code, URI, T, Code)
 when Code >= 500, Code < 600 ->
   aura:send({urn, <<"c">>, uid('5xx', URI)}, T, 1);

trace(http, ttfb, URI, T, X) ->
   aura:send({urn, <<"g">>, uid(ttfb, URI)}, T, X);

trace(http, ttmr, URI, T, X) ->
   aura:send({urn, <<"g">>, uid(ttmr, URI)}, T, X).

