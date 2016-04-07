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
%%   rest api - 
-module(zephyrus_dashboard).
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
   [{'text', 'html'}].

%%
%%
'GET'(_, {Url, _Head, Env}) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   case typhoon:lookup(Id, [{r, 1}]) of
      {error, unity} ->
         {303, [{'Location', location(Url, Id)}], <<>>};

      {ok,   Entity} ->
         case ambitz:entity(service, Entity) of
            undefined ->
               404;
            _         ->
               % {'Access-Control-Allow-Origin', <<"*">>}
               file:read_file(
                  % filename:join([code:priv_dir(zephyrus), htdoc, "index.html"])
                  filename:join([code:priv_dir(zephyrus), htdoc, "scenario.html"])
               )
         end
   end.


%%
%% return location header
location(Url, Id) ->
   uri:s( uri:segments([scenario, Id], Url) ).
