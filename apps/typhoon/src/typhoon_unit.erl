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
  ,idle/3
  ,active/3
]).

%%-----------------------------------------------------------------------------
%%
%% factory
%%
%%-----------------------------------------------------------------------------

start_link(Name, Spec) ->
   pipe:start_link(?MODULE, [Name, Spec], []).

init([Name, Spec]) ->
   random:seed(os:timestamp()),
   Scenario = scenario:compile(Spec),
   erlang:process_flag(trap_exit, true),
   erlang:send(self(), request),
   tempus:timer(scenario:t(Scenario), expired),
   {ok, idle, 
      #{
         name      => Name,
         scenario  => Scenario,
         telemetry => aura:socket(), 
         peer      => typhoon:peer(Name)
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
idle(request, Pipe, #{scenario := Scenario0} = State0) ->
   clue:inc({typhoon, req}),
   {Req, Scenario1} = scenario:eval(Scenario0),
   case request(Req, State0) of
      {active,  State1} ->
         {next_state, active, State1#{scenario => Scenario1}};
      {request, State1} ->
         {next_state, idle, State1#{scenario => Scenario1}}
   end;

idle(expired, _, State) ->
   {stop, normal, State};

idle(_, _Pipe, State) ->
   {next_state, idle, State}.


%%
%%
active({http, _, {Code, _Text, Head, _Env}}, _, State) ->
   telemetry(os:timestamp(), {http, status, Code}, State),
   clue(State),
   {next_state, active, State#{recv => q:new()}};

active({trace, T, Msg}, _, State) ->
   telemetry(T, Msg, State),
   {next_state, active, State#{}};

active({http, _, eof}, _, #{recv := Recv, scenario := Scenario0} = State) ->
   erlang:send(self(), request),
   Scenario1 = scenario:accept(erlang:iolist_to_binary(q:list(Recv)), Scenario0),
   {next_state, idle, State#{scenario => Scenario1}};

active({http, _, Pckt}, _, #{recv := Recv} = State) ->
   {next_state, active, State#{recv => q:enq(Pckt, Recv)}};

active(expired, _, State) ->
   {stop, normal, State};

active({sidedown, _, _}, _, State) ->
   %% @todo: Do we need to reflect into telemetry connection error? 
   %%        How To ?
   erlang:send(self(), request),
   {next_state, idle, State};

active(_, _Pipe, State) ->
   {next_state, active, State}.

%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
%% create socket if it is not exists
socket(Url, #{sock := Sock} = State) ->
   %% @todo: monitor socket process
   case erlang:is_process_alive(Sock) of
      false ->
         socket(Url, maps:remove(sock, State));
      true  ->
         Sock         
   end;

socket(Url, _State) ->
   knet:socket(Url, [{trace, self()}]).

%%
%%
request(#{type := thinktime, urn := Urn, t := T}, State) ->
   tempus:timer(T, request),
   {request, State};

request(#{type := protocol,  urn := Urn, packet := List}, State) ->
   clue:inc({typhoon, req}),
   {_Mthd, Url, _Head} = hd(List),
   Sock = socket(Url, State),
   lists:foreach(
      fun(Pack) ->
         pipe:send(Sock, Pack)
      end,
      List
   ),
   {active, State#{urn => Urn, sock => Sock}}.


%%
%% 
telemetry(T, Value, #{urn := Urn, peer := Peers, telemetry := Sock}) ->
   aura:send(Sock, Peers, {Urn, T, Value}).

%%
%%
clue(#{name := Name, peer := Peers, telemetry := Sock}) ->
   aura:clue(Sock, Peers, Name).



