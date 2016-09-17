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
%%   pure functional network i/o library
%%
-module(njord).
-author('dmitry.kolesnikov@zalando.fi').
-compile({parse_transform, monad}).

-export([start/0]).
-export([t/0]).
-export([http/0]).

%%
%% start application
start() ->
   applib:boot(?MODULE, []).


t() ->
   do([m_state ||
      X <- a(),
      Y <- b(X),
      return(Y)
   ]).

a() ->
   do([m_http || 
      _ /= new("urn:http:xxx"),
      _ /= url("https://api.zalando.com/"),
      _ /= header("Accept-Language", "de-DE"),
      _ /= header("Connection", "keep-alive"),
      njord:http()
   ]).

b(I) ->
   do([m_http || 
      _ /= new("urn:http:yyy"),
      _ /= url("https://api.zalando.com/articles"),
      _ /= header("Accept-Language", "de-DE"),
      _ /= header("Connection", "keep-alive"),
      njord:http()
   ]).


%%
%% network i/o  
http() ->
   m_http:request().





