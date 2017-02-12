%% @doc
%%   pipe complaint adapter of knet library
-module(njord_adapter).
-behaviour(pipe).

-export([
   start_link/2
  ,start_link/1
  ,init/1
  ,free/2
  ,handle/3
]).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

start_link(Uri) ->
   start_link(Uri, []).

start_link(Uri, Opts) ->
   pipe:start_link(?MODULE, [Uri, Opts], []).

init([Uri, Opts]) ->
   {ok, handle,
      #{
         sock => knet:socket(Uri, [{trace, self()}|Opts]),
         urn  => undefined
      }
   }.

free(_Reason, #{sock := Sock}) ->
   knet:close(Sock).

%%%----------------------------------------------------------------------------   
%%%
%%% pipe
%%%
%%%----------------------------------------------------------------------------   

handle({trace, Urn}, Pipe, State) ->
   pipe:ack(Pipe, ok),
   {next_state, handle, State#{urn => Urn}};

handle({trace, _,   _}, _Pipe, #{urn := undefined} = State) ->
   {next_state, handle, State};

handle({trace, T, Msg}, _Pipe, #{urn := Urn} = State) ->
   % socket trace message
   trace(Urn, T, Msg),
   {next_state, handle, State};   

handle({Prot, Sock, Ingress}, Pipe, #{sock := Sock} = State) ->
   % ingress message from network 
   pipe:b(Pipe, {Prot, self(), Ingress}),
   {next_state, handle, State};

handle(Egress, Pipe, State) ->
   pipe:b(Pipe, Egress),
   {next_state, handle, State}.


%%
%% @todo: how to abstract telemetry aggregation from connection management ?
%%        we need to abstract aura api so that we can tunnel telemetry
trace(Urn, T, {tcp, connect, X}) ->
   aura:send(urn({urn, <<"g">>, <<"tcp">>}, Urn), T, X);
trace(Urn, T, {tcp, {connect, _}, X}) ->
   aura:send(urn({urn, <<"g">>, <<"tcp">>}, Urn), T, X);

trace(Urn, T, {tcp, packet, X}) ->
   aura:send(urn({urn, <<"c">>, <<"pack">>}, Urn), T, 1),
   aura:send(urn({urn, <<"g">>, <<"pack">>}, Urn), T, X);
trace(Urn, T, {tcp, {packet, _}, X}) ->
   aura:send(urn({urn, <<"c">>, <<"pack">>}, Urn), T, 1),
   aura:send(urn({urn, <<"g">>, <<"pack">>}, Urn), T, X);

trace(Urn, T, {ssl, handshake, X}) ->
   aura:send(urn({urn, <<"g">>, <<"ssl">>}, Urn), T, X);
trace(Urn, T, {ssl, {handshake, _}, X}) ->
   aura:send(urn({urn, <<"g">>, <<"ssl">>}, Urn), T, X);

trace(Urn, T, {ssl, packet, X}) ->
   aura:send(urn({urn, <<"c">>, <<"pack">>}, Urn), T, 1),
   aura:send(urn({urn, <<"g">>, <<"pack">>}, Urn), T, X);
trace(Urn, T, {ssl, {packet, _}, X}) ->
   aura:send(urn({urn, <<"c">>, <<"pack">>}, Urn), T, 1),
   aura:send(urn({urn, <<"g">>, <<"pack">>}, Urn), T, X);

trace(Urn, _T, {ssl, ca, _X}) ->
   ok;
trace(Urn, _T, {ssl, {ca, _}, _X}) ->
   ok;

trace(Urn, _T, {ssl, peer, _X}) ->
   ok;
trace(Urn, _T, {ssl, {peer, _}, _X}) ->
   ok;

trace(Urn, T, {http, code, Code})
 when Code >= 200, Code < 300 ->
   aura:send(urn({urn, <<"c">>, <<"2xx">>}, Urn), T, 1);
trace(Urn, T, {http, {code, _}, Code})
 when Code >= 200, Code < 300 ->
   aura:send(urn({urn, <<"c">>, <<"2xx">>}, Urn), T, 1);

trace(Urn, T, {http, code, Code})
 when Code >= 300, Code < 400 ->
   aura:send(urn({urn, <<"c">>, <<"3xx">>}, Urn), T, 1);
trace(Urn, T, {http, {code, _}, Code})
 when Code >= 300, Code < 400 ->
   aura:send(urn({urn, <<"c">>, <<"3xx">>}, Urn), T, 1);

trace(Urn, T, {http, code, Code})
 when Code >= 400, Code < 500 ->
   aura:send(urn({urn, <<"c">>, <<"4xx">>}, Urn), T, 1);
trace(Urn, T, {http, {code, _}, Code})
 when Code >= 400, Code < 500 ->
   aura:send(urn({urn, <<"c">>, <<"4xx">>}, Urn), T, 1);

trace(Urn, T, {http, code, Code})
 when Code >= 500, Code < 600 ->
   aura:send(urn({urn, <<"c">>, <<"5xx">>}, Urn), T, 1);
trace(Urn, T, {http, {code, _}, Code})
 when Code >= 500, Code < 600 ->
   aura:send(urn({urn, <<"c">>, <<"5xx">>}, Urn), T, 1);

trace(Urn, T, {http, ttfb, X}) ->
   aura:send(urn({urn, <<"g">>, <<"ttfb">>}, Urn), T, X);
trace(Urn, T, {http, {ttfb, _}, X}) ->
   aura:send(urn({urn, <<"g">>, <<"ttfb">>}, Urn), T, X);

trace(Urn, T, {http, ttmr, X}) ->
   aura:send(urn({urn, <<"g">>, <<"ttmr">>}, Urn), T, X);
trace(Urn, T, {http, {ttmr, _}, X}) ->
   aura:send(urn({urn, <<"g">>, <<"ttmr">>}, Urn), T, X).


%%
%%
urn(Base, Urn) ->
   uri:join([uri:schema(Urn), uri:path(Urn)], Base).
