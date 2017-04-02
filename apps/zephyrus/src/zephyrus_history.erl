%%
%%   Copyright 2016 Zalando SE
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
%%   rest api - history of scenario
-module(zephyrus_history).
-author('dmitry.kolesnikov@zalando.fi').

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
   [{'application', 'json'}].

%%
%%
'GET'(_Type, _Msg, {_Url, _Head, Env}) ->
   Id  = lens:get(lens:pair(<<"id">>), Env),
   Urn = {urn, <<"c">>, <<"history:", (scalar:s(Id))/binary>>},
   {ok, List} = aura:stream(Urn, history(Urn)),
   {ok, jsx:encode(List)}.


history(Urn) ->
   A = os:timestamp(),
   B = tempus:sub(A, 30 * 24 * 3600),
   fun(FD) ->
      stream:map(
         fun({T, X}) ->
            [
               {t, tempus:s(T)},
               {duration,    X}
            ]
         end,
         chronolog:stream(FD, Urn, {A, B})
      )
   end.
