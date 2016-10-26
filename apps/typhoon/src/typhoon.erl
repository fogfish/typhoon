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

-include_lib("ambitz/include/ambitz.hrl").

-export([start/0]).
%%
%% management interface
-export([
   signup/2
  ,profile/2
  ,scenario/2


  ,define/3
  ,lookup/2
  ,remove/2

  ,peer/1
  ,run/1
  ,unit/1
  ,attr/1
]).
%%
%% data interface
-export([
   fd/0
  ,stream/2
]).


%%
%% data types
-type urn()    :: {urn, _, _}.
-type spec()   :: binary().  %% application/erlang
-type opts()   :: [_].


% -type(json()   :: [{binary(), _}]).

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
%% sign up a new user
-spec signup(urn(), opts()) -> ok.

signup({urn, user, _} = User, Opts) ->
   Profile = crdts:update(uri:path(User), crdts:new(lwwreg)),
   Spec    = {typhoon_user, start_link, [Profile]},
   {ok, _} = ambitz:spawn(typhoon, uri:s(User), Spec, Opts),
   ok.

%%
%% check user profile
-spec profile(urn(), opts()) -> {ok, _} | {error, not_found}.

profile({urn, user, _} = User, Opts) ->
   case ambitz:get(typhoon, uri:s(User), profile, Opts) of
      {ok, #entity{val = undefined}} ->
         {error, not_found};
      {ok, #entity{val = Profile}} ->
         {ok, crdts:value(Profile)}
   end.

%%
%% check user scenario
-spec scenario(urn(), opts()) -> {ok, _} | {error, not_found}.

scenario({urn, user, _} = User, Opts) ->
   case ambitz:get(typhoon, uri:s(User), scenario, Opts) of
      {ok, #entity{val = undefined}} ->
         {error, not_found};
      {ok, #entity{val = Scenario}} ->
         {ok, crdts:value(Scenario)}
   end.

%%
%% defines load scenario to the cluster, 
%% takes unique name of test scenario and its specification,
%% returns cluster descriptor entity
-spec define(urn(), spec(), opts()) -> {ok, _}.

define({urn, _, _} = Id, Spec, Opts) ->
   User  = {urn, user, uri:schema(Id)},
   ScenarioA = crdts:update(Id, crdts:new(gsets)),
   Actor = {typhoon_scenario, start_link, [scalar:atom(uri:path(Id)), Spec]},
   {ok, _} = ambitz:spawn(typhoon, uri:s(Id), Actor, Opts),
   {ok, #entity{val = ScenarioB}} = ambitz:put(typhoon, uri:s(User), scenario, ScenarioA, Opts),
   {ok, crdts:value(ScenarioB)}.


%%
%% lookup load scenario using it's unique name
%% returns cluster descriptor entity, including scenario specification
-spec lookup(urn(), opts()) -> {ok, ambitz:entity()}.

lookup({urn, _, _} = Id, Opts) ->
   ambitz:lookup(typhoon, uri:s(Id), Opts).


%%
%% remove load scenario from cluster
-spec remove(urn(), opts()) -> ok.

remove({urn, _, _} = Id, Opts) ->
   ambitz:free(typhoon, uri:s(Id), Opts).


%%
%% run load scenario, the scenario will terminate automatically after timeout
-spec run(urn()) -> ok | {error, _}.

run({urn, _, _} = Id) ->
   {ok, #entity{val = CRDT}} = ambitz:whereis(typhoon, uri:s(Id), [{r, 1}]),
   Pids = crdts:value(CRDT),
   Pid  = lists:nth(random:uniform(length(Pids)), Pids),
   pipe:call(Pid, run).





%%
%% return list of peer(s) nodes (ip addresses) 
-spec peer(urn()) -> [_].

peer(Id) ->
   [addr(Vnode) || Vnode <- ek:successors(typhoon, scalar:s(Id)), 
      ek:vnode(type, Vnode) == primary].

addr(Vnode) ->
   %% @todo: it would fail if cluster uses FQDN
   [_, Host] = binary:split(ek:vnode(node, Vnode), <<$@>>),
   {ok, IP}  = inet_parse:address(scalar:c(Host)),
   IP.





%%
%% return number of active load units
-spec unit(urn()) -> {ok, integer()} | {error, any()}.

unit({urn, _, _} = Id) ->
   {ok, #entity{val = CRDT}} = ambitz:whereis(typhoon, uri:s(Id), [{r, 3}]),
   lists:sum(
      lists:map(
         fun(Pid) -> pipe:ioctl(Pid, n) end,
         crdts:value(CRDT)
      )
   ).

%%
%% return number of active load units
-spec attr(urn()) -> {ok, [_]} | {error, any()}.

attr({urn, _, _} = Id) ->
   {ok, #entity{val = CRDT}} = ambitz:whereis(typhoon, uri:s(Id), [{r, 3}]),
   Pid = hd( crdts:value(CRDT) ),
   {ok, pipe:ioctl(Pid, attr)}.


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
-spec stream(urn(), fun( (chronolog:fd()) -> datum:stream() )) -> {ok, list()} | {error, any()}.

stream(Id, Gen) ->
   {ok, #entity{vnode = Vnode}} = ambitz:lookup(typhoon, Id, [{r, 3}]),
   Node  = erlang:node(ek:vnode(peer, lists:nth(random:uniform(length(Vnode)), Vnode))),
   pipe:call({typhoon_peer, Node}, {stream, Gen}, 300000).


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

