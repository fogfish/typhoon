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
%%   example of raw socket work-load scenario 
-module(socket).
-compile({parse_transform, monad}).

%%
%% scenario attributes
-export([title/0, t/0, n/0]).

%%
%% scenario actions
-export([run/1]).


%%%----------------------------------------------------------------------------   
%%%
%%% attributes
%%%
%%%----------------------------------------------------------------------------   

%%
%% human readable scenario title
title() ->
   "Example TCP/IP Workload".

%%
%% duration of each scenario session in milliseconds,
%% the session is aborted when the timeout is expired.
t() ->
   120000.

%%
%% number of concurrent session to spawn in the cluster.
n() ->
   1.

%%%----------------------------------------------------------------------------   
%%%
%%% actions
%%%
%%%----------------------------------------------------------------------------   

%%
%% execute scenario, the operation is repeated  until `t()` is expired. 
run(_Config) ->
   do([m_state ||
      _ <- request(),  %% execute raw socket i/o 
      A <- request(),  %% execute raw socket i/o
      return(A)
   ]).


request() ->
   do([m_sock ||
      %% create new socket i/o handler
      _ /= new("ssl://api.zalando.com"),

      %% use raw socket to send   
      _ /= send(<<"GET / HTTP/1.1\r\n">>),
      _ /= send(<<"Host: api.zalando.com\r\n">>),
      _ /= send(<<"\r\n">>),

      %% use raw socket to recv
      A /= recv(),
      B /= recv(),
      return([A, B])
   ]).
