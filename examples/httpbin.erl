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
%%   example of work-load scenario 
-module(httpbin).
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
   "Example Workload Scenario".

%%
%% duration of each scenario session in milliseconds,
%% the session is aborted when the timeout is expired.
t() ->
   120000.

%%
%% number of concurrent session to spawn in the cluster.
n() ->
   30.

%%%----------------------------------------------------------------------------   
%%%
%%% actions
%%%
%%%----------------------------------------------------------------------------   

%%
%% The scenario entry-point, it defines a sequence of requests to execute towards SUT.
%% Actions are composition of functions, each function builds and executes protocol 
%% operation (e.g. HTTP request), the result of protocol operation is returned to next 
%% function and so on. We are using IO-monad to isolate side-effect and protocol stack 
%% from scenario developers.
run(_) -> 
   do([m_state ||
      % the first action is HTTP GET request
      _ <- get_ip(),

      % the second and third actions are nested chains of requests
      _ <- usecase_a(),
      _ <- usecase_b(),
      return(_)
   ]).

usecase_a() ->
   do([m_state ||
      % the use-case executes three HTTP GET requests 
      _ <- get_ip(),
      _ <- get_ip(),
      _ <- get_ip(),

      % the result of last HTTP GET request is feed to HTTP POST request
      _ <- post(_),
      return(_)
   ]).

usecase_b() ->
   do([m_state ||
      % the use-case executes three HTTP GET requests 
      _ <- get_ip(),
      _ <- get_ip(),
      X <- get_ip(),

      % the result of last HTTP GET request is fed to HTTP POST request
      _ <- post(X),

      % the result of previous HTTP POST request is fed again to HTTP POST request
      _ <- post(X),
      return(_)
   ]).


get_ip() ->
   do([m_http ||
      _ /= new("http://127.0.0.1:8888/ip"),
      _ /= method('GET'),
      _ /= header("Connection", "keep-alive"),
      _ /= request(),
      _ =< scenario:decode(_),
      return( scenario:lens([origin], _) )
   ]).

post(IP) ->
   do([m_http ||
      _ /= new("http://127.0.0.1:8888/post"),
      _ /= method('POST'),
      _ /= header("Transfer-Encoding", "chunked"),
      _ /= header("Content-Type", "text/plain"),
      _ /= header("Connection", "keep-alive"),
      _ /= payload(IP),
      _ /= request(),
      _ =< scenario:decode(_), 
      return( scenario:lens([data], _) )
   ]).

