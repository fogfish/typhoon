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
   start_link/2
  ,init/1
  ,free/2
  ,handle/3
]).

%%-----------------------------------------------------------------------------
%%
%% factory
%%
%%-----------------------------------------------------------------------------

start_link(Scenario, T) ->
   pipe:start_link(?MODULE, [Scenario, T], []).

init([Scenario, T]) ->
   kill_process_at(T),
   erlang:send(self(), request),
   {ok, handle, 
      #{
         scenario  => Scenario,
         config    => config(Scenario),
         context   => #{so => [{trace, {{urn, root, Scenario}, aura:adapter()}}]}
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
handle(request, _, #{scenario := Scenario, config := Config, context := Context0} = State) ->
   Ta  = os:timestamp(),
   [_|Context1] = ( Scenario:run(hd(Config)) )(Context0),
   Urn  = {urn, <<"g">>, <<"scenario:", (scalar:s(Scenario))/binary>>},
   Tb  = os:timestamp(),
   aura:send(Urn, Tb, tempus:u(tempus:sub(Tb, Ta))),
   erlang:send(self(), request),
   {next_state, handle, State#{context => Context1}};

handle(expired, _, State) ->
   {stop, normal, State};

handle(_, _, State) ->
   %% ignore any side effect from script
   {next_state, handle, State}.
 

%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
%% configure scenario execution
config(Scenario) ->
   case lens:get(lens:pair(init, undefined), Scenario:module_info(exports)) of
      0 ->
         Fun = Scenario:init(),
         Fun(#{});

      _ ->
         [undefined]
   end.

%%
%%
kill_process_at(T) ->
   case os:timestamp() of
      X when X >= T ->
         erlang:send(self(), expired);
      X ->
         tempus:timer(tempus:sub(T, X), expired)
   end.

