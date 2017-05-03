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
%%   rest api - health check
-module(zephyrus_health).
-author('dmitry.kolesnikov@zalando.fi').

-compile({parse_transform, category}).
-compile({parse_transform, monad}).

-export([
   allowed_methods/1,
   content_provided/1, 
   'GET'/3
]).

%%
%%
allowed_methods(_Req) ->
   ['GET'].

%%
%%
content_provided(_Req) ->
   [{'application', 'json'}].

%%
%%
'GET'(_Type, _Msg, {_Url, _Head, Env}) ->
   [$^ ||
      fmap(lens:get(lens:pair(<<"check">>), Env)),
      health(_),
      fmap(jsx:encode(_))
   ].

health(<<"peer">>) ->
   % active peers 
   Peer = [erlang:node() | erlang:nodes()],
   {ok, [scalar:s(X) || X <- Peer]};

health(<<"sys">>) ->
   do([m_either ||
      Peers <- health(<<"peer">>),
      IOrps <- aura:clue({urn, <<"c">>, <<"sys:rps">>}),
      IOcap <- aura:clue({urn, <<"c">>, <<"sys:capacity">>}),
      Scenario <- aura:clue({urn, <<"c">>, <<"sys:scenario">>}),
      return([
         {time, tempus:s(os:timestamp())},
         {peers, Peers},
         {rps, IOrps}, 
         {failure, IOrps - IOcap}, 
         {scenario, Scenario}
      ])
   ]).


