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
%%   rest api - 
-module(zephyrus_dashboard).
-author('dmitry.kolesnikov@zalando.fi').
-include_lib("ambitz/include/ambitz.hrl").


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
   [{'text', 'html'}].

%%
%%
'GET'(_Type, _Msg, {Url, _Head, Env}) ->
   case uri:segments(Url) of
      [] ->
         html(<<"typhoon">>);

      [<<"dashboard">>] -> 
         html(<<"profile">>);

      [<<"dashboard">>, UI | _] ->
         html(UI);

      _ ->
         scenario(lens:get(lens:pair(<<"id">>), Env))
   end.   


html(File) ->
   file:read_file(
      filename:join([code:priv_dir(zephyrus), htdoc, <<File/binary, ".html">>])
   ).


scenario(Id) ->
   {ok, #entity{val = Val}} = typhoon:get({urn, root, Id}, [{r, 1}]),
   case crdts:value(Val) of
      undefined ->
         {404, <<"Not Found">>};

      _Entity ->
         file:read_file(
            filename:join([code:priv_dir(zephyrus), htdoc, "scenario.html"])
         )
   end.
