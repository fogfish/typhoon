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
   erlang:send(self(), request),
   {ok, handle, 
      #{
         scenario  => Scenario,
         peer      => typhoon:peer(Scenario)
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
handle(request, _, #{scenario := Scenario, peer := Peer} = State) ->
   Fun = Scenario:run(),
   _   = Fun(#{pool => fun netpool/1, peer => Peer}),
   erlang:send(self(), request),
   {next_state, handle, State};

handle(expired, _, State) ->
   {stop, normal, State}.
 

%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
%% selector of net i/o pool
netpool(Url) ->
   Id = scalar:atom(uri:s(uri:suburi(<<>>, Url))),
   case erlang:whereis(Id) of
      undefined ->
         typhoon_net_sup:spawn(Id, Url),
         netpool(Url);
      Pid ->
         Pid
   end.      
