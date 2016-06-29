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
-module(typhoon_net_sup).
-behaviour(supervisor).
-author('dmitry.kolesnikov@zalando.fi').

-export([
   start_link/0, init/1
]).
-export([spawn/3]).

-define(CHILD(Type, I),            {I,  {I, start_link,   []}, transient, 5000, Type, dynamic}).
-define(CHILD(Type, I, Args),      {I,  {I, start_link, Args}, transient, 5000, Type, dynamic}).
-define(CHILD(Type, ID, I, Args),  {ID, {I, start_link, Args}, transient, 5000, Type, dynamic}).

%%-----------------------------------------------------------------------------
%%
%% supervisor
%%
%%-----------------------------------------------------------------------------

start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).
   
init([]) ->   
   {ok,
      {
         {one_for_one, 10000, 1},
         [
         ]
      }
   }.


%%
%% spawn new socket pool
spawn(Id, 'keep-alive', Uri) ->
   pid(
      supervisor:start_child(?MODULE, 
         ?CHILD(supervisor, Id, pq, [Id, netpool(Uri, lifo)])
      )
   );

spawn(Id, disposable, Uri) ->
   pid(
      supervisor:start_child(?MODULE, 
         ?CHILD(supervisor, Id, pq, [Id, netpool(Uri, spawn)])
      )
   ).


pid({ok, _}) -> ok;
pid({error, {already_started, _}}) -> ok.


%%
netpool(Uri, Type) ->
   [
      {capacity,  inf},
      {worker,    {typhoon_net, [Uri]}},
      {strategy,  Type}
   ].



