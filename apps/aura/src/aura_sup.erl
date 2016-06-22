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
-module(aura_sup).
-behaviour(supervisor).
-author('dmitry.kolesnikov@zalando.fi').

-export([
   start_link/0, init/1
]).

-define(CHILD(Type, I),            {I,  {I, start_link,   []}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, I, Args),      {I,  {I, start_link, Args}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, ID, I, Args),  {ID, {I, start_link, Args}, permanent, 5000, Type, dynamic}).

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
            %% persistence layer
            ?CHILD(worker,     aura_storage)

            %% transport layer
           ,?CHILD(supervisor, aura_egress_sup)
           ,?CHILD(supervisor, aura_ingress_sup)

            %% data stream processing 
           ,?CHILD(supervisor, aura_stream_sup, pts, pts(aura_stream))
           ,?CHILD(supervisor, aura_sensor_sup, pts, pts(aura_sensor))
         ]
      }
   }.

%%
%%
pts(Mod) ->
   [
      Mod,
      [
         'read-through',
         {keylen,    inf},
         {entity,    Mod},
         {factory,   temporary}
      ]
   ].

