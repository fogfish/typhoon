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
try
   Stream = stream:map(
      fun({T, X}) -> [tempus:s(T), lists:max(X)] end,
      chronolog:join([carrier(Url, Env), series(Url, Env)])
   ),
   {ok, jsx:encode(stream:list(Stream))}
catch _:R ->
   lager:error("~p ~p~n", [R, erlang:get_stacktrace()]), 
   {ok, jsx:encode([])}
end.

%%
%% build a series stream
series(Url, Env) ->
   Chronon = chronon(Url),
   Id  = pair:x(<<"id">>, Env),
   Urn = uri:new( pair:x(<<"ticker">>, Env) ),
   A   = t(pair:x(<<"from">>, Env)),
   B   = t(pair:x(<<"to">>,   Env)),
   Ref = case 
      {uri:q(<<"filter">>, false, Url), uri:q(<<"status">>, false, Url), uri:q(<<"metric">>, false, Url)}
   of
      {false,  false, false} ->
         typhoon:series(Id, Urn, Chronon, {A, B});
      {<<"pth", Pth/binary>>, false, false} ->
         typhoon:series(Id, Urn, {pth, scalar:f(Pth)}, Chronon, {A, B});
      {Filter, false, false} ->
         typhoon:series(Id, Urn, scalar:a(Filter), Chronon, {A, B});
      {false,  Code,  false} ->
         typhoon:status(Id, Urn, scalar:i(Code), Chronon, {A, B});
      {false,  false,  true} ->
         typhoon:metric(Id, Urn, Chronon, {A, B})
   end,
   receive 
      {Ref, List} ->
         stream:build(List)
   after 60000 ->
      exit(timeout)
   end.
   
%%
%% built a carrier stream, stream filled by 0 at each chronon
carrier(Url, Env) -> 
   Chronon = chronon(Url),
   A       = t(pair:x(<<"from">>, Env), Chronon),
   B       = t(pair:x(<<"to">>,   Env), Chronon),
   stream:takewhile(
      fun(X) -> X =/= eos end,
      stream:unfold(
         fun
         (X) when X < B ->
            {{X,0}, tempus:add(X, tempus:t(s, Chronon))};
         (X) ->
            {eos, {X,0}}
         end,
         A
      )
   ).

%%
%%
chronon(Url) ->
   scalar:i( uri:q(<<"chronon">>, 86400, Url) ).

%%
%%
t(T, Chronon) ->
   tempus:discrete(tempus:t(s, scalar:i(T)), Chronon).

t(T) ->
   tempus:t(s, scalar:i(T)).
