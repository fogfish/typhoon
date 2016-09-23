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
%%   load unit - generates load towards SUT
-module(typhoon_unit).
-behaviour(pipe).
-author('dmitry.kolesnikov@zalando.fi').

-include("typhoon.hrl").

-export([
   start_link/1
  ,init/1
  ,free/2
  ,handle/3
]).

%%-----------------------------------------------------------------------------
%%
%% factory
%%
%%-----------------------------------------------------------------------------

start_link(Scenario) ->
   pipe:start_link(?MODULE, [Scenario], []).

init([Scenario]) ->
   random:seed(os:timestamp()),
   tempus:timer(Scenario:t(), expired),
   Peer = typhoon:peer(Scenario),
   erlang:send(self(), request),
   {ok, handle, 
      #{
         scenario  => Scenario,
         peer      => Peer,
         config    => config(Scenario, Peer),
         context   => #{pool => fun netpool/2, peer => Peer}
      }
   }.

free(_Reason, _) ->
   ok.

%%-----------------------------------------------------------------------------
%%
%% pipe
%%
%%-----------------------------------------------------------------------------

%%
%%
handle(request, _, #{scenario := Scenario, peer := Peer, config := Config, context := Context0} = State) ->
   Ta  = os:timestamp(),
   [_|Context1] = (Scenario:run(Config))(Context0),
   Urn  = {urn, <<"g">>, <<"scenario:", (scalar:s(Scenario))/binary>>},
   Tb  = os:timestamp(),
   aura:send(Urn, Tb, tempus:u(tempus:sub(Tb, Ta))),
   erlang:send(self(), request),
   {next_state, handle, State#{context => Context1}};

handle(expired, _, State) ->
   {stop, normal, State}.
 

%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
%% selector of net i/o pool (@todo: re-factor code here)
netpool(Url, Header) ->
   case lists:keyfind('Connection', 1, Header) of
      {'Connection', <<"close">>} ->
         Id = scalar:atom(uri:s(uri:suburi(<<"disposable">>, Url))),
         case erlang:whereis(Id) of
            undefined ->
               typhoon_net_sup:spawn(Id, disposable, Url),
               netpool(Url, Header);
            Pid ->
               Pid
         end;
      
      _ ->
         Id = scalar:atom(uri:s(uri:suburi(<<"keep-alive">>, Url))),
         case erlang:whereis(Id) of
            undefined ->
               typhoon_net_sup:spawn(Id, 'keep-alive', Url),
               netpool(Url, Header);
            Pid ->
               Pid
         end
   end.

%%
%% configure scenario execution
config(Scenario, Peer) ->
   case lens:get(lens:pair(init, undefined), Scenario:module_info(exports)) of
      0 ->
         Fun = Scenario:init(),
         Fun(#{pool => fun netpool/2, peer => Peer});

      _ ->
         undefined
   end.

