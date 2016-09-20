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

%% @todo: make thinktime global service of njord
%% @todo: how to handle trace ?

%%
%% start application
start() ->
   applib:boot(?MODULE, []).


t() ->
   do([m_state ||
      X <- a(),
      Y <- b(X),
      % Y <- c(),
      return(Y)
   ]).

a() ->
   do([m_http || 
      _ /= new("urn:http:xxx"),
      _ /= url("https://api.zalando.com/"),
      _ /= header("Accept-Language", "de-DE"),
      _ /= header("Connection", "keep-alive"),
      _ /= get(),
      return(_)
   ]).

b(_) ->
   do([m_http || 
      _ /= new("urn:http:yyy"),
      _ /= url("https://api.zalando.com/articles"),
      _ /= header("Accept-Language", "de-DE"),
      _ /= header("Connection", "keep-alive"),
      _ /= get(),
      return(_)
   ]).


c() ->
   do([m_sock ||
      _ /= new(),
      _ /= url("ssl://api.zalando.com"),
      _ /= thinktime(10),
      _ /= send(<<"GET / HTTP/1.1\r\n">>),
      _ /= thinktime(10),      
      _ /= send(<<"Host: api.zalando.com\r\n">>),
      _ /= thinktime(10),
      _ /= send(<<"\r\n">>),
      _ /= thinktime(10),
      _ /= recv(),
      return(_)
   ]).


