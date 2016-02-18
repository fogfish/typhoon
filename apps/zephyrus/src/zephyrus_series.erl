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
   content_accepted/1,
   'GET'/2
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
content_accepted(_Req) ->
   [].

%%
'GET'(_, {Url, _Head, Env}) ->
   Id  = lens:get(lens:pair(<<"id">>), Env),
   Urn = uri:new( lens:get(lens:pair(<<"urn">>), Env) ),
   A   = t(lens:get(lens:pair(<<"from">>), Env)),
   B   = t(lens:get(lens:pair(<<"to">>),   Env)),
   Chronon = scalar:i( uri:q(<<"chronon">>, 60, Url) ),
   case lens:get(lens:pair(<<"kpi">>), Env) of
      
      %%
      %% http transaction per second
      <<"200">> ->
         stream(Id, tps(Urn, 200, Chronon, {A, B}));
      <<"300">> ->
         stream(Id, tps(Urn, 300, Chronon, {A, B}));
      <<"400">> ->
         stream(Id, tps(Urn, 400, Chronon, {A, B}));
      <<"500">> ->
         stream(Id, tps(Urn, 500, Chronon, {A, B}));

      %%
      %% any request per second
      <<"rps">> ->
         stream(Id, rps(Urn, Chronon, {A, B}));

      %%
      %% mean sampled value
      <<"mean">> ->
         stream(Id, mean(Urn, Chronon, {A, B}));

      %%
      %% max sampled value
      <<"max">> ->
         stream(Id, maxx(Urn, Chronon, {A, B}));

      %%
      %% percentile sampled value
      <<"p90">> ->
         stream(Id, pth(Urn, 0.90, Chronon, {A, B}));

      <<"p95">> ->
         stream(Id, pth(Urn, 0.95, Chronon, {A, B}));

      <<"p99">> ->
         stream(Id, pth(Urn, 0.99, Chronon, {A, B}))
   end.

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
%% transactions per time unit generator function
tps(Req, Code, Chronon, Range) ->
   fun(FD) ->
      cubism(
         chronolog:join([
            carrier(Chronon, Range),
            smpu(Chronon, http(Code, chronolog:stream(FD, Req, Range) ) ) 
         ])
      )       
   end.

%%
%% transactions per time unit generator function
rps(Req, Chronon, Range) ->
   fun(FD) ->
      cubism(
         chronolog:join([
            carrier(Chronon, Range),
            smpu(Chronon, chronolog:stream(FD, Req, Range) ) 
         ])
      )       
   end.

%%
%% mean value per time unit generator function
mean(Req, Chronon, Range) ->
   fun(FD) ->
      cubism(
         chronolog:join([
            carrier(Chronon, Range),
            mean(Chronon, chronolog:stream(FD, Req, Range) ) 
         ])
      )       
   end.

%%
%% mean value per time unit generator function
maxx(Req, Chronon, Range) ->
   fun(FD) ->
      cubism(
         chronolog:join([
            carrier(Chronon, Range),
            maxx(Chronon, chronolog:stream(FD, Req, Range) ) 
         ])
      )       
   end.

%%
%% percentile per time unit generator function
pth(Req, Pth, Chronon, Range) ->
   fun(FD) ->
      cubism(
         chronolog:join([
            carrier(Chronon, Range),
            pth(Chronon, Pth, chronolog:stream(FD, Req, Range) ) 
         ])
      )       
   end.

%%
%% filter http telemetry with given status code 
http(Code, Stream) ->
   stream:filter(
      fun({_, X}) ->
         X >= Code andalso X < ((Code div 100) + 1) * 100
      end,
      Stream
   ).

%%
%% build measurement per unit metric (e.g. RPS)
smpu(Chronon, Stream) ->
   chronolog:scan(
      fun(X) -> length(X) / Chronon end,
      Chronon,
      Stream
   ).

%%
%% calculate mean value on chronon
mean(Chronon, Stream) ->
   chronolog:scan(
      fun(X) -> lists:sum(X) div length(X) end,
      Chronon,
      Stream
   ).

%%
%% calculate max value on chronon
maxx(Chronon, Stream) ->
   chronolog:scan(
      fun(X) -> lists:max(X) end,
      Chronon,
      Stream
   ).

%%
%% calculate percentile value on chronon
pth(Chronon, Pth, Stream) ->
   chronolog:scan(
      fun(X) -> lists:nth(round(length(X) * Pth), lists:sort(X)) end,
      Chronon,
      Stream
   ).


%%
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
%%
t(T) ->
   tempus:t(s, scalar:i(T)).

