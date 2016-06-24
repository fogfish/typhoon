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
%%   rest api - return scenario attributes
-module(zephyrus_attribute).
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
%%
'GET'(_, {Url, _Head, Env}) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   _  = scalar:i(uri:q(<<"r">>, 1, Url)),
   case typhoon:attr(Id) of
      {error, unity} ->
         {303, [{'Location', uri:s(Url)}], <<>>};
      
      {ok,   Spec} ->
         Json = [
            {capacity,     attribute(capacity, Spec)}
           ,{availability, attribute(availability, Spec)}
           ,{latency,      attribute(latency, Spec)}
           |Spec
         ],
         {ok, jsx:encode(Json)}
   end.


attribute(capacity, Spec) ->
   http(<<"2xx">>, Spec);

attribute(availability, Spec) ->
   Ht200 = http(<<"2xx">>, Spec),
   Ht400 = http(<<"4xx">>, Spec),
   Ht500 = http(<<"5xx">>, Spec),
   case (Ht200 + Ht400 + Ht500) of
      0.0 -> 0.0;
      X   -> 100 * Ht200 / X
   end;

attribute(latency, Spec) ->
   Scenario = lens:get(lens:pair(id), Spec),
   {ok, X}  = aura:clue(
      {urn, <<"g">>, <<"scenario:", (scalar:s(Scenario))/binary>>}
   ),
   X.

%%
urn(Base, Urn) ->
   uri:join([uri:schema(Urn), uri:path(Urn)], Base).

%%
%% read http attribute
http(Code, Spec) ->
   lists:sum(
      lists:map(
         fun(Urn) ->
            {ok, X} = aura:clue(urn({urn, <<"c">>, Code}, uri:new(Urn))),
            X
         end,
         lens:get(lens:pair(urn), Spec)
      )
   ).








% 'GET'(_, {_Url, _Head, Env}) ->
%    Urn = lens:get(lens:pair(<<"urn">>), Env),
%    {ok, Val} = aura:clue(uri:new(Urn)),
%    {ok, jsx:encode(Val)}.
