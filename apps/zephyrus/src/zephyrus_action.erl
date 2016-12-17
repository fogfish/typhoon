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
%%   rest api - execute scenario actions
-module(zephyrus_action).
-author('dmitry.kolesnikov@zalando.fi').

-export([
   allowed_methods/1,
   content_provided/1, 
   'GET'/2
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
'GET'(_, {Url, _Head, Env}) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   case lens:get(lens:pair(<<"action">>), Env) of

      %%
      %% spawn load scenario 
      <<"spawn">> ->
         _ = typhoon:run({urn, root, Id}),
         {202, [{'Location',  uri:s(uri:segments([Id], Url))}], <<>>};

      %%
      %% ping runing workers (concurrent units producing load)
      <<"ping">> ->
         {200, 
            jsx:encode([
               {processes, typhoon:unit(Id)},
               {rps,       clue:get(Id)}
            ])
         }
   end.
