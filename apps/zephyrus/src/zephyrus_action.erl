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
%%   rest api - execute scenario action
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
   [{'*', '*'}].

%%
%%
'GET'(_, {_Url, _Head, Env}) ->
   Id = pair:x(<<"id">>, Env),
   case pair:x(<<"action">>, Env) of
      <<"spawn">> ->
         _  = typhoon:run(Id),
         {302, [{'Location', <<$/, Id/binary>>}], <<>>};
      <<"ping">> ->
         {200, jsx:encode(typhoon:unit(Id))};
      <<"seed">> ->
         Seed = [erlang:node() | erlang:nodes()], 
         {200, jsx:encode([scalar:s(X) || X <- Seed])}
   end.
