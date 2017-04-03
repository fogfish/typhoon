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

handle({trace, T, Scenario, _, _, {Event, URI}, Value}, _Pipe, State) ->
   trace(uri:schema(URI), Event, Scenario, URI, T, Value),
   {next_state, handle, State}.

uid(Type, URI) ->
   uri:s( uri:schema([Type, uri:schema(URI)], URI) ).

trace(tcp, connect, _, URI, T, X) ->
   aura:send({urn, <<"g">>, uid(connect, URI)}, T, X);

trace(tcp, packet, _, URI, T, X) ->
   aura:send({urn, <<"c">>, uid(packet, URI)}, T, 1), %% packet per second
   aura:send({urn, <<"g">>, uid(packet, URI)}, T, X); %% packet size 

trace(ssl, connect, _, URI, T, X) ->
   aura:send({urn, <<"g">>, uid(connect, URI)}, T, X);

trace(ssl, handshake, _, URI, T, X) ->
   aura:send({urn, <<"g">>, uid(handshake, URI)}, T, X);

trace(ssl, packet, _, URI, T, X) ->
   aura:send({urn, <<"c">>, uid(packet, URI)}, T, 1),
   aura:send({urn, <<"g">>, uid(packet, URI)}, T, X);

trace(ssl, ca, _, _, _, _) ->
   ok;
trace(ssl, peer, _, _, _, _) ->
   ok;

trace(http, code, {urn, _, Scenario}, URI, T, Code)
 when Code >= 200, Code < 300 ->
   aura:send({urn, <<"c">>, <<(scalar:s(Scenario))/binary, ":capacity">>}, T, 1),
   aura:send({urn, <<"c">>, <<"sys:capacity">>}, T, 1),
   aura:send({urn, <<"c">>, uid('2xx', URI)}, T, 1);

trace(http, code, _, URI, T, Code)
 when Code >= 300, Code < 400 ->
   aura:send({urn, <<"c">>, uid('3xx', URI)}, T, 1);

trace(http, code, _, URI, T, Code)
 when Code >= 400, Code < 500 ->
   aura:send({urn, <<"c">>, uid('4xx', URI)}, T, 1);

trace(http, code, _, URI, T, Code)
 when Code >= 500, Code < 600 ->
   aura:send({urn, <<"c">>, uid('5xx', URI)}, T, 1);

trace(http, ttfb, {urn, _, Scenario}, URI, T, X) ->
   aura:send({urn, <<"c">>, <<(scalar:s(Scenario))/binary, ":rps">>}, T, 1),
   aura:send({urn, <<"c">>, <<"sys:rps">>}, T, 1),
   aura:send({urn, <<"g">>, uid(ttfb, URI)}, T, X);

trace(http, ttmr, _, URI, T, X) ->
   aura:send({urn, <<"g">>, uid(ttmr, URI)}, T, X).

