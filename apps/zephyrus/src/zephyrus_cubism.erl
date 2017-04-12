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
-module(zephyrus_cubism).
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
'GET'(_Type, _Msg, {Url, _Head, Env}) ->
   Urn = uri:new( uri:unescape( lens:get(lens:pair(<<"urn">>), Env) ) ),
   A   = t(lens:get(lens:pair(<<"from">>), Env)),
   B   = t(lens:get(lens:pair(<<"to">>),   Env)),
   Chronon = scalar:i( uri:q(<<"chronon">>, 60, Url) ),
   stream(Urn, value(Urn, Chronon, {A, B})).

%%
stream(Urn, Gen) ->
   case aura:stream(Urn, Gen) of
      {error, _} ->
         500;

      {ok, List} ->
         {ok, jsx:encode(List)}
   end.

%% transactions per time unit generator function
value(Req, Chronon, Range) ->
   fun(FD) ->
      cubism(
         chronolog:join([
            carrier(Chronon, Range),
            mean(Chronon, chronolog:stream(FD, Req, Range) ) 
         ])
      )       
   end.

%% calculate mean value on chronon
mean(Chronon, Stream) ->
   chronolog:scan(
      % fun(X) -> lists:sum(X) div length(X) end,
      fun(X) -> lists:max(X) end,
      Chronon,
      Stream
   ).

%% return time series stream compatible with cubism.js
cubism(Stream) ->
   stream:map(
      fun({T, X}) -> 
         [tempus:s(T), lists:max(X)] 
      end,
      Stream
   ).

%%
%% built a carrier stream, stream filled by 0 at each chronon
carrier(Chronon, {A, B}) -> 
   Ta = tempus:discrete(A, Chronon),
   Tb = tempus:discrete(B, Chronon),
   stream:takewhile(
      fun(X) -> X =/= eos end,
      stream:unfold(
         fun
         (X) when X < Tb ->
            {{X,0}, tempus:add(X, tempus:t(s, Chronon))};
         (X) ->
            {eos, {X,0}}
         end,
         Ta
      )
   ).

%%
t(T) ->
   tempus:t(s, scalar:i(T)).


