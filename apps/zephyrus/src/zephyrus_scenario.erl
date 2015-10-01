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
%%   rest api - scenario management
-module(zephyrus_scenario).
-author('dmitry.kolesnikov@zalando.fi').

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   'GET'/2,
   'PUT'/3,
   'DELETE'/2
]).

%%
allowed_methods(_Req) ->
   ['GET', 'PUT', 'DELETE'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
content_accepted(_Req) ->
   [{application, json}].

%%
%%
'GET'(_, {_Url, _Head, Env}) ->
   case typhoon:lookup(pair:x(<<"id">>, Env)) of
      {error, Reason} ->
         Reason;
      Entity ->
         case ambitz:entity(service, Entity) of
            undefined ->
               not_found;

            {typhoon_scenario, start_link, [_Name, Spec]} ->
               {ok, jsx:encode(Spec)}
         end
   end.

%%
%%
'PUT'(_, {_Url, _Head, Env}, Msg) ->
   case typhoon:define(pair:x(<<"id">>, Env), jsx:decode(Msg)) of
      {error, Reason} ->
         Reason;
      _Entity ->
         ok
   end.

%%
%%
'DELETE'(_, {_Url, _Head, Env}) ->
   case typhoon:remove(pair:x(<<"id">>, Env)) of
      {error, Reason} ->
         Reason;
      _Entity ->
         ok
   end.

