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

-include_lib("ambitz/include/ambitz.hrl").
-compile({parse_transform, category}).

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   'GET'/3,
   'PUT'/3,
   'DELETE'/3
]).

%%
allowed_methods(_Req) ->
   ['GET', 'PUT', 'DELETE'].

%%
content_provided(_Req) ->
   [{application, erlang}, {application, json}].

%%
content_accepted(_Req) ->
   [{application, erlang}, {application, json}].

%%
%%
'GET'(_Type, _Msg, {Url, _Head, Env}) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   _  = scalar:i(uri:q(<<"r">>, 1, Url)),
   [$^ ||
      typhoon:attr({urn, root, Id}),
      unit(jsx:encode(_)) 
   ].

%%
%%
'PUT'({_, {application, erlang}}, Spec, {Url, _Head, Env}) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   W  = scalar:i(uri:q(<<"w">>, 1, Url)),
   [$^ ||
      typhoon:put({urn, root, Id}, erlang:iolist_to_binary(Spec), [{w, W}]),
      typhoon:attr({urn, root, Id}),
      unit(jsx:encode(_)) 
   ];

'PUT'({_, {application, json}}, Data, {Url, _Head, Env}) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   W  = scalar:i(uri:q(<<"w">>, 1, Url)),
   Json = [{scalar:atom(Key), Val} || {Key, Val} <- jsx:decode(iolist_to_binary(Data))],
   Spec = spec([{id, Id} | Json]),
   [$^ ||
      typhoon:put({urn, root, Id}, erlang:iolist_to_binary(Spec), [{w, W}]),
      typhoon:attr({urn, root, Id}),
      unit(jsx:encode(_)) 
   ].


%%
%%
'DELETE'(_Type, _Msg, {Url, _Head, Env}) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   W  = scalar:i(uri:q(<<"w">>, 1, Url)),   
   {ok, _} = typhoon:remove({urn, root, Id}, [{w, W}]),
   {ok, jsx:encode(Id)}.


%%
spec(Json) ->
   {ok, File} = file:read_file(filename:join([code:priv_dir(zephyrus), "typhoon.swirl"])),
   Fun0 = swirl:f(File),
   Fun1 = Fun0(undefined),
   Fun1(Json).
