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
%%   rest api - time series data
-module(zephyrus_series).
-author('dmitry.kolesnikov@zalando.fi').


-export([
   allowed_methods/1,
   content_provided/1, 
   'GET'/3
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
'GET'(_Type, _Msg, {_Url, _Head, Env}) ->
   Id  = lens:get(lens:pair(<<"id">>), Env),
   Urn = uri:new( uri:unescape( lens:get(lens:pair(<<"urn">>), Env) ) ),
   A   = t(lens:get(lens:pair(<<"from">>), Env)),
   B   = t(lens:get(lens:pair(<<"to">>),   Env)),
   stream(Id, id(Urn, {A, B})).

%%
%%
stream(Id, Gen) ->
   case typhoon:stream(Id, Gen) of
      {error, _} ->
         500;

      {ok, List} ->
         {ok, jsx:encode(List)}
   end.

%%
%% identity stream
id(Req, Range) ->
   fun(FD) ->
      stream:map(
         fun({T, X}) -> 
            [tempus:s(T), X] 
         end,
         chronolog:stream(FD, Req, Range)
      )
   end.

%%
%%
t(T) ->
   tempus:t(s, scalar:i(T)).
