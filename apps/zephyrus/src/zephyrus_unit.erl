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
-module(zephyrus_unit).
-author('dmitry.kolesnikov@zalando.fi').

-include_lib("ambitz/include/ambitz.hrl").

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
   [{text, html}].

%%
content_accepted(_Req) ->
   [{application, erlang}].


%%
%%
'PUT'(_Type, Spec, {Url, _Head, Env}) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   W  = scalar:i(uri:q(<<"w">>, 1, Url)), 
   {ok, _} = typhoon:put({urn, root, Id}, Spec, [{w, W}]),
   {ok, _} = typhoon:scenario({urn, user, root}, {urn, root, Id}, [{w, W}]),
   201.

%%
%%
'DELETE'(_Type, _Msg, {Url, _Head, Env}) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   W  = scalar:i(uri:q(<<"w">>, 1, Url)),   
   {ok,_} = typhoon:remove({urn, root, Id}, [{w, W}]),
   200.

%%
%%
'GET'(_Type, _Msg, {Url, _Head, Env}) ->
   Id   = lens:get(lens:pair(<<"id">>), Env),
   Pass = typhoon:once({urn, root, Id}),
   {ok, File} = file:read_file(filename:join([code:priv_dir(zephyrus), "unit.html"])),
   Fun0 = swirl:f(scalar:c(File)),   
   Fun1 = Fun0(undefined),
   case is_passed(Pass) of
      [true] ->
         {200, Fun1(#{list => jsonify(Pass)})};
      _ ->
         {500, Fun1(#{list => jsonify(Pass)})}
   end.

is_passed(Pass) ->
   lists:usort(
      lists:map(
         fun(#{pass := X}) -> X end,
         lists:flatten([Y || #{unit := Y} <- Pass]) 
      )
   ).


jsonify([#{unit := Unit} = X | Tail]) ->
   [X#{unit => jsonify_unit(Unit)} | jsonify(Tail)];
jsonify([]) ->
   [].

jsonify_unit([#{lens := Lens} = X | Tail]) ->
   [X#{lens => jsonify_lens(Lens)}| jsonify_unit(Tail)];
jsonify_unit([X | Tail]) ->
   [X| jsonify_unit(Tail)];
jsonify_unit([]) ->
   [].

jsonify_lens([{X} | Tail]) ->
   [<<${, (scalar:s(X))/binary, $}>> | jsonify_lens(Tail)];
jsonify_lens([{X, Y} | Tail]) ->
   [<<${, (scalar:s(X))/binary, $,, (scalar:s(Y))/binary, $}>> | jsonify_lens(Tail)];
jsonify_lens([X | Tail]) ->
   [X | jsonify_lens(Tail)];
jsonify_lens([]) ->
   [].


