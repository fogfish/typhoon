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
   [{application, erlang}].

%%
content_accepted(_Req) ->
   [{application, erlang}].

%%
%%
'GET'(_, {Url, _Head, Env}) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   R  = scalar:i(uri:q(<<"r">>, 1, Url)),   
   case typhoon:lookup(Id, [{r, R}]) of
      {error, unity} ->
         {303, [{'Location', uri:s(Url)}], <<>>};
      
      {ok,   Entity} ->
         case ambitz:entity(service, Entity) of
            undefined ->
               404;

            Service   ->
               Spec = lens:get(lens:t3(), lens:tl(), lens:hd(), Service),
               {ok, Spec}
         end      
   end.

%%
%%
'PUT'(_, {Url, _Head, Env}, Scenario) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   W  = scalar:i(uri:q(<<"w">>, 1, Url)), 
   case typhoon:define(Id, Scenario, [{w, W}]) of
      {error, unity} ->
         {303, [{'Location', uri:s(Url)}], <<>>};

      {ok,   Entity} ->
         {201, json(Entity)}
   end.

%%
%%
'DELETE'(_, {Url, _Head, Env}) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   W  = scalar:i(uri:q(<<"w">>, 1, Url)),   
   case typhoon:remove(Id, [{r, W}, {w, W}]) of
      {error, unity} ->
         {303, [{'Location', uri:s(Url)}], <<>>};

      {ok,   Entity} ->
         {200, json(Entity)}
   end.

%%
%% return status descriptor
json(Entity) ->
   Primary = length([X || X <- ambitz:entity(vnode, Entity), ek:vnode(type, X) =:= primary]),
   Handoff = length([X || X <- ambitz:entity(vnode, Entity), ek:vnode(type, X) =:= handoff]),
   jsx:encode([{primary, Primary}, {handoff, Handoff}]).

