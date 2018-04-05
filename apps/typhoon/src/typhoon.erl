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
-author('dmitry.kolesnikov@gmail.com').

% -include_lib("ambitz/include/ambitz.hrl").
% -compile({parse_transform, category}).


-export([start/0]).
-export([
  test/1
]).

% %%
% %% management interface
% -export([
%    put/3
%   ,get/2
%   ,remove/2

%   ,signup/2
%   ,profile/2
%   ,scenario/2

%   ,peer/1
%   ,run/1
%   ,abort/1
%   ,unit/1
%   ,attr/1
% ]).

% %%
% %% data types
% -type urn()    :: {urn, _, _}.
% -type opts()   :: [_].


%%
%% RnD node start
start() ->
   applib:boot(?MODULE, code:where_is_file("sys.config")).


%%
%% evaluate local sketches
-spec test(_) -> {ok, _}.

test(Dir) ->
  typhoon_local_test:run(Dir).



% %%%----------------------------------------------------------------------------   
% %%%
% %%% management interface
% %%%
% %%%----------------------------------------------------------------------------   

% %%
% %% create a new workload scenario
% -spec put(urn(), binary(), opts()) -> {ok, _} | {error, _}.

% put({urn, User, _} = Key, Spec, Opts) ->
%    [$^||
%       scenario_spawn(Key, Spec, Opts),
%       typhoon_kv:append(scenario, {urn, user, User}, Key, Opts)
%    ].

% scenario_spawn(Key, Spec, Opts) ->
%    ambitz:spawn(typhoon, uri:s(Key),
%       {typhoon_scenario, start_link, [scalar:atom(uri:path(Key)), Spec]},
%       Opts
%    ).

% %%
% %% read content of workload scenario
% -spec get(urn(), opts()) -> {ok, ambitz:entity()}.

% get({urn, _, _} = Key, Opts) ->
%    ambitz:lookup(typhoon, uri:s(Key), Opts).


% %%
% %% remove workload scenario
% -spec remove(urn(), opts()) -> {ok, _} | {error, _}.

% remove({urn, User, _} = Key, Opts) ->
%    [$^||
%       typhoon_kv:remove(scenario, {urn, user, User}, Key, Opts),
%       scenario_free(Key, Opts)
%    ].

% scenario_free(Key, Opts) ->
%    ambitz:free(typhoon, uri:s(Key), Opts).


% %%
% %% sign up a new user
% -spec signup(urn(), opts()) -> ok.

% signup({urn, user, _} = User, Opts) ->
%    Profile = crdts:update(uri:path(User), crdts:new(lwwreg)),
%    Spec    = {typhoon_user, start_link, [Profile]},
%    {ok, _} = ambitz:spawn(typhoon, uri:s(User), Spec, Opts),
%    ok.

% %%
% %% read user profile
% -spec profile(urn(), opts()) -> {ok, _} | {error, not_found}.

% profile({urn, user, _} = User, Opts) ->
%    case ambitz:get(typhoon, uri:s(User), profile, Opts) of
%       {ok, #entity{val = undefined}} ->
%          {error, not_found};
%       {ok, #entity{val = Profile}} ->
%          {ok, crdts:value(Profile)}
%    end.

% %%
% %% read user scenario
% -spec scenario(urn(), opts()) -> {ok, _} | {error, not_found}.

% scenario({urn, user, _} = User, Opts) ->
%    case ambitz:get(typhoon, uri:s(User), scenario, Opts) of
%       {ok, #entity{val = undefined}} ->
%          {error, not_found};
%       {ok, #entity{val = Scenario}} ->
%          {ok, crdts:value(Scenario)}
%    end.
   


% %%
% %% run load scenario, the scenario will terminate automatically after timeout
% -spec run(urn()) -> ok | {error, _}.

% run({urn, _, _} = Id) ->
%    {ok, #entity{val = CRDT}} = ambitz:whereis(typhoon, uri:s(Id), [{r, 1}]),
%    Pids = crdts:value(CRDT),
%    Pid  = lists:nth(rand:uniform(length(Pids)), Pids),
%    pipe:call(Pid, run).


% %%
% %% abort load scenario
% -spec abort(urn()) -> ok | {error, _}.

% abort({urn, _, _} = Id) ->
%    {ok, #entity{val = Val}} = typhoon:get(Id, [{w, 3}]),
%    typhoon:remove(Id, [{w, 3}]),
%    {_, _, [Mod, Spec]} = crdts:value(Val),
%    lists:foreach(
%       fun(Node) ->
%          _ = rpc:call(Node, code, purge, [Mod]),
%          _ = rpc:call(Node, code, delete, [Mod])
%       end,
%       [erlang:node() | erlang:nodes()]
%    ),
%    %% Note: this is not nice but we need to delay re-start of scenario
%    %%       the ultimate fix in the pipeline
%    timer:sleep(5000),
%    typhoon:put(Id, Spec, [{w, 3}]).



% %%
% %% return list of peer(s) nodes (ip addresses) 
% -spec peer(urn()) -> [_].

% peer(Id) ->
%    [addr(Vnode) || Vnode <- ek:successors(typhoon, scalar:s(Id)), 
%       ek:vnode(type, Vnode) == primary].

% addr(Vnode) ->
%    %% @todo: it would fail if cluster uses FQDN
%    [_, Host] = binary:split(ek:vnode(node, Vnode), <<$@>>),
%    {ok, IP}  = inet_parse:address(scalar:c(Host)),
%    IP.





% %%
% %% return number of active load units
% -spec unit(urn()) -> {ok, integer()} | {error, any()}.

% unit({urn, _, _} = Id) ->
%    {ok, #entity{val = CRDT}} = ambitz:whereis(typhoon, uri:s(Id), [{r, 3}]),
%    lists:sum(
%       lists:map(
%          fun(Pid) -> pipe:ioctl(Pid, n) end,
%          crdts:value(CRDT)
%       )
%    ).

% %%
% %% return static attributes about scenario
% -spec attr(urn()) -> {ok, [_]} | {error, any()}.

% attr({urn, _, _} = Id) ->
%    {ok, #entity{val = CRDT}} = ambitz:whereis(typhoon, uri:s(Id), [{r, 3}]),
%    Pid = hd( crdts:value(CRDT) ),
%    {ok, pipe:ioctl(Pid, attr)}.


% %%%----------------------------------------------------------------------------   
% %%%
% %%% private
% %%%
% %%%----------------------------------------------------------------------------   

