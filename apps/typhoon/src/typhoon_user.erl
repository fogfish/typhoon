%%
%%   Copyright 2016 Zalando SE
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
%%   user coordinator process
-module(typhoon_user).
-behaviour(pipe).
-author('dmitry.kolesnikov@zalando.fi').

-export([
   start_link/2
  ,init/1
  ,free/2
  ,handle/3
]).

-record(state, {
   profile  = undefined :: crdts:crdt(),
   scenario = undefined :: crdts:crdt()
}).

%%%----------------------------------------------------------------------------   
%%%
%%% Factory
%%%
%%%----------------------------------------------------------------------------   

start_link(_Vnode, Profile) ->
   pipe:start_link(?MODULE, [Profile], []).

init([Profile]) ->
   lager:notice("typhoon [user]: sign up ~p~n", [crdts:value(Profile)]),
   {ok, handle, 
      #state{
         profile  = Profile,
         scenario = crdts:new(gsets)
      }
   }.

free(_Reason, _State) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% pipe
%%%
%%%----------------------------------------------------------------------------   

handle({get, profile}, Pipe, #state{profile = Profile} = State) ->
   pipe:ack(Pipe, {ok, Profile}),
   {next_state, handle, State};

handle({put, scenario, ScenarioB}, Pipe, #state{scenario = ScenarioA} = State) ->
   ScenarioC = crdts:join(ScenarioA, ScenarioB),
   pipe:ack(Pipe, {ok, ScenarioC}),
   {next_state, handle, State#state{scenario = ScenarioC}};

handle({get, scenario}, Pipe, #state{scenario = Scenario} = State) ->
   pipe:ack(Pipe, {ok, Scenario}),
   {next_state, handle, State};

handle(_Msg, _Tx, State) ->
   {next_state, handle, State}.

