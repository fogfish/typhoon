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
-export([title/0, t/0, n/0, urn/0]).

%%
%% scenario entry point
-export([run/0]).


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
   600000.

%%
%% number of concurrent session to spawn in the cluster.
n() ->
   30.

%%
%% list of request identifiers used at latency visualization
urn() ->
   [
      "urn:http:httpbin:get",
      "urn:http:httpbin:post"
   ].


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
run() -> 
   [{do, 'Mio'} ||
      % the first action is HTTP GET request
      A <- get_ip(),

      % the second and third actions are nested chains of requests
      B <- usecase_a(),
      C <- usecase_b(),
      return(C)
   ].

usecase_a() ->
   [{do, 'Mio'} ||
      % the use-case executes three HTTP GET requests 
      A <- get_ip(),
      B <- get_ip(),
      C <- get_ip(),

      % the result of last HTTP GET request is feed to HTTP POST request
      Y <- post(C),
      return(Y)
   ].

usecase_b() ->
   [{do, 'Mio'} ||
      % the use-case executes three HTTP GET requests 
      A <- get_ip(),
      B <- get_ip(),
      C <- get_ip(),

      % the result of last HTTP GET request is fed to HTTP POST request
      Y <- post(C),

      % the result of previous HTTP POST request is fed again to HTTP POST request
      Z <- post(Y),
      return(Z)
   ].


get_ip() ->
   [{do, 'Mid'} ||
      A <- scenario:new("urn:http:httpbin:get"),
      B <- scenario:method('GET', A),
      C <- scenario:url("http://127.0.0.1:8888/ip", B),
      D <- scenario:header("Connection", "keep-alive", C),
      scenario:request([origin], D)
   ].

post(Y) ->
   [{do, 'Mid'} ||
      A <- scenario:new("urn:http:httpbin:post"),
      B <- scenario:method('POST', A),
      C <- scenario:url("http://127.0.0.1:8888/post", B),
      D <- scenario:header("Content-Type", "text/plain", C),
      E <- scenario:header("Connection", "keep-alive", D),
      G <- scenario:payload(Y, E),
      scenario:request(D)
   ].

