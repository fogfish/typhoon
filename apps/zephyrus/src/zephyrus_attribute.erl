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

-compile({parse_transform, category}).

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
         [m_either ||
            Capacity <- capacity(Id),
            Availability <- availability(Id),
            Latency <- latency(Id),
            cats:unit(Spec),
            cats:unit([{capacity, Capacity} | _]), 
            cats:unit([{availability, Availability} | _]),
            cats:unit([{latency, Latency} | _]),
            unit(jsx:encode(_))
         ]
   end.

capacity(Scenario) ->
   aura:clue({urn, <<"c">>, <<Scenario/binary, ":capacity">>}).

availability(Scenario) ->
   [m_either || 
      RPS <- aura:clue({urn, <<"c">>, <<Scenario/binary, ":rps">>}),
      Cap <- aura:clue({urn, <<"c">>, <<Scenario/binary, ":capacity">>}),
      cats:unit(case RPS of X when X > 0 -> Cap / RPS; _ -> 0 end),
      unit(_)  
   ].

latency(Scenario) ->
   aura:clue({urn, <<"g">>, <<"scenario:", (scalar:s(Scenario))/binary>>}).

