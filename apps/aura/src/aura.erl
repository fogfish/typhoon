%%
%%   Copyright 2015 Zalando SE
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
-module(aura).

-export([start/0]).
-export([
   encode/3
  ,decode/1
  ,send/3
]).

%% udp port base
-define(PORT,  20100).

%%
%% RnD node start
start() ->
   applib:boot(?MODULE, code:where_is_file("dev.config")).


%%
%%  
encode(Urn, T, X)
 when is_integer(X) ->
   <<(uri:s(Urn))/binary, $|, (scalar:s(tempus:u(T)))/binary, $,, (scalar:s(X))/binary>>;

encode(Urn, T, {tcp, connect, X}) ->
   encode(uri:schema(<<"tcp">>, Urn),  T, X);

encode(Urn, T, {tcp, packet, X}) ->
   encode(uri:schema(<<"pack">>, Urn),  T, X);

encode(Urn, T, {ssl, handshake, X}) ->
   encode(uri:schema(<<"ssl">>, Urn),  T, X);

encode(Urn, T, {ssl, packet, X}) ->
   encode(uri:schema(<<"pack">>, Urn),  T, X);

encode(Urn, T, {http, ttfb, X}) ->
   encode(uri:schema(<<"ttfb">>, Urn), T, X);

encode(Urn, T, {http, ttmr, X}) ->
   encode(uri:schema(<<"ttmr">>, Urn), T, X);

encode(Urn, T, {http, status, X}) ->
   encode(uri:schema(<<"http">>, Urn), T, X);

encode(Urn, T, {_, _, _} = X) ->
   encode(Urn, T, tempus:u(X)).


%%
%%
decode(Pack) ->
   [Urn, Val] = binary:split(Pack, <<$|>>),
   [  T,   X] = binary:split(Val,  <<$,>>),
   {uri:new(Urn), tempus:t(u, scalar:i(T)), scalar:i(X)}.

%%
%%
send(Sock, Peer, Pack) ->
   %% @todo: validate perf of format -> queue prefix + urn might impact performance
   gen_udp:send(Sock, Peer, port(), <<"auraq:", Pack/binary>>).

port() ->
   ?PORT + random:uniform(10) - 1.
   

