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
   'GET'/3
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
%%
'GET'(_Type, _Msg, {Url, _Head, Env}) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   _  = scalar:i(uri:q(<<"r">>, 1, Url)),
   case typhoon:attr({urn, root, Id}) of
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
      X when X > 0 -> 
         100 * Ht200 / X;
      _ ->
         0.0
   end;

attribute(latency, Spec) ->
   Scenario = lens:get(lens:pair(id), Spec),
   {ok, X}  = aura:clue(
      {urn, <<"g">>, <<"scenario:", (scalar:s(Scenario))/binary>>}
   ),
   X.

%%
%% read http attribute
http(Code, Spec) ->
   lists:sum(
      lists:map(
         fun(Urn) ->
            Uri = uri:new(Urn),
            Uid = uri:s( uri:schema([Code, uri:schema(Uri)], Uri) ),
            {ok, X} = aura:clue({urn, <<"c">>, Uid}),
            X
         end,
         lens:get(lens:pair(urls), Spec)
      )
   ).
