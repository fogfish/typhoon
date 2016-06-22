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
         peer => peer(Urn)
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
handle({T, X}, _, #{urn := Urn, sock := Sock, peer := Peer} = State) ->
   pipe:send(Sock, {Peer, {Urn, T, X}}),   
   {next_state, handle, State}.


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
