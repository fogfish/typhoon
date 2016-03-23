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
%%   native interface distributed system stress and load testing tool
-module(typhoon).
-author('dmitry.kolesnikov@zalando.fi').

-export([start/0]).
%%
%% management interface
-export([
   define/3
  ,lookup/2
  ,remove/2
  ,run/1
  ,unit/1
]).
%%
%% data interface
-export([
   fd/0
  ,stream/2
  % ,series/4
  % ,series/5
  % ,metric/4
  % ,status/5
]).


%%
%%
-type(id()     :: binary()).
-type(json()   :: [{binary(), _}]).

% -type(filter() :: mean | max | {pth, float()}).

%%
%% RnD node start
start() ->
   applib:boot(?MODULE, code:where_is_file("dev.config")).

%%%----------------------------------------------------------------------------   
%%%
%%% management interface
%%%
%%%----------------------------------------------------------------------------   

%%
%% defines load scenario to the cluster, 
%% takes unique name of test scenario and its specification,
%% returns cluster descriptor entity
-spec define(id(), json(), list()) -> {ok, ambitz:entity()} | {error, _}.

define(Id, Spec, Opts) ->
   ambitz:spawn(
      ambitz:entity(ring, typhoon, 
         ambitz:entity(service, {typhoon_scenario, start_link, [Id, Spec]},
            ambitz:entity(Id)
         )
      ),
      Opts
   ).


%%
%% lookup load scenario using it's unique name
%% returns cluster descriptor entity, including scenario specification
-spec lookup(id(), list()) -> {ok, ambitz:entity()} | {error, _}.

lookup(Id, Opts) ->
   ambitz:lookup(
      ambitz:entity(ring, typhoon, 
         ambitz:entity(Id)
      ),
      Opts
   ).

%%
%% remove load scenario from cluster
-spec remove(id(), list()) -> {ok, ambitz:entity()} | {error, _}.

remove(Id, Opts) ->
   case    
      ambitz:lookup(
         ambitz:entity(ring, typhoon, 
            ambitz:entity(Id)
         ),
         Opts
      )
   of
      {error, _} = Error ->
         Error;
      {ok, Entity} ->
         ambitz:free(Entity, Opts)
   end.

%%
%% run load scenario, the scenario will terminate automatically after timeout
-spec run(id()) -> ok | {error, _}.

run(Id) ->
   case
      ambitz:whereis(
         ambitz:entity(ring, typhoon, 
            ambitz:entity(Id)
         ),
         [{r, 1}]
      )
   of
      {error, _} = Error ->
         Error;
      {ok, Entity} ->
         Pids = ambitz:entity(service, Entity),
         pipe:call(lists:nth(random:uniform(length(Pids)), Pids), run)
   end.

%%
%% return number of active load units
-spec(unit/1 :: (id()) -> {ok, integer()} | {error, any()}).

unit(Id) ->
   case
      ambitz:whereis(
         ambitz:entity(ring, typhoon, ambitz:entity(Id)),
         [{r, 1}]
      )
   of
      {error, _} = Error ->
         Error;
      
      {ok, Entity} ->
         lists:sum(
            lists:map(
               fun(X) -> pipe:ioctl(X, n) end,
               ambitz:entity(service, Entity)
            )
         )
   end.



%%%----------------------------------------------------------------------------   
%%%
%%% data interface
%%%
%%%----------------------------------------------------------------------------   

%%
%% file descriptor to time series data-base
-spec fd() -> chronolog:fd().

fd() ->
   pipe:ioctl(typhoon_peer, fd).


%%
%% takes a generator function and produce telemetry stream
-spec stream(id(), fun( (chronolog:fd()) -> datum:stream() )) -> {ok, list()} | {error, any()}.

stream(Id, Gen) ->
   case
      ambitz:lookup(
         ambitz:entity(ring, typhoon, 
            ambitz:entity(Id)
         )
      )
   of
      {error, _} = Error ->
         Error;

      {ok, Entity} ->
         Vnode = ambitz:entity(vnode, Entity),
         Node  = erlang:node(ek:vnode(peer, lists:nth(random:uniform(length(Vnode)), Vnode))),
         pipe:call({typhoon_peer, Node}, {stream, Gen}, 30000)
   end.
