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
%% @doc
%%   client side publisher of stream data (e.g. map operation)  
-module(aura_stream).
-behaviour(pipe).

-export([
   start_link/2,
   init/1,
   free/2,
   handle/3
]).

-define(A, 0.2).


%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

start_link(Ns, Urn) ->
   pipe:start_link(?MODULE, [Ns, Urn], []).


init([Ns, {urn, _, _} = Urn]) ->
   ok = pns:register(Ns, Urn, self()),
   random:seed(os:timestamp()),
   {ok, handle,
      #{
         urn  => Urn,
         sock => socket(),
         peer => peer(Urn),
         t    => {0, 0, 0},
         x    => 0.0
      }
   }.

free(_, _State) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% pipe
%%%
%%%----------------------------------------------------------------------------   

%% @todo ttl timeout 

%%
%%
handle({{A, B, _}, X}, Pipe, #{t := {A, B, _}} = State) ->
   pipe:ack(Pipe, ok),
   {next_state, handle, update(X, State), 500};

handle({{A0, A1, _} = A, _} = X, Pipe, #{t := B} = State)
 when A > B ->
   State0 = append(State),
   handle(X, Pipe, State0#{t := {A0, A1, 0}});

handle({{_, _, _}, _}, Pipe, State) ->
   %% ignore out-dated
   pipe:ack(Pipe, ok),
   {next_state, handle, State, 500};

handle(timeout, _Pipe, State) ->
   {next_state, handle, append(State)}.



%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% return list of peers (ip addresses) responsible for sensor
peer(Urn) ->
   [addr(Vnode) 
      || Vnode <- ek:successors(aura, uri:s(Urn)), 
         ek:vnode(type, Vnode) == primary].

%% @todo: it would fail if cluster uses FQDN
addr(Vnode) ->
   [_, Host] = binary:split(ek:vnode(node, Vnode), <<$@>>),
   {ok, IP}  = inet_parse:address(scalar:c(Host)),
   IP.

%%
%% allocate egress socket
socket() ->
   Socks = [erlang:element(2, X) || X <- supervisor:which_children(aura_egress_sup)],
   lists:nth( random:uniform(length(Socks)), Socks ).


%%
update(X, #{urn := {urn, <<"g">>, _}, x := X0} = State) ->
   State#{x => ?A * X + (1 - ?A) * X0};

update(X, #{urn := {urn, <<"c">>, _}, x := X0} = State) ->
   State#{x => X0 + X};

update(_, State) ->
   State.


%%
append(#{t := {0, 0, 0}} = State) ->
   State;
append(#{urn := Urn, sock := Sock, peer := Peer, t := T, x := X} = State) ->
   pipe:send(Sock, {Peer, {Urn, T, erlang:trunc(X)}}), 
   State#{t => {0, 0, 0}, x => 0.0}.



