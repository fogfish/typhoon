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
-module(ex_httpbin).
-compile({parse_transform, monad}).

%%
%% scenario attributes
-export([t/0, n/0, urn/0]).

%%
%% scenario entry point
-export([run/0]).


%%%----------------------------------------------------------------------------   
%%%
%%% attributes
%%%
%%%----------------------------------------------------------------------------   

%%
%% duration of each scenario session in milliseconds,
%% the session is aborted when the timeout is expired.
t() ->
   600000.

%%
%% number of concurrent session to spawn in the cluster.
n() ->
   10.

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
   [{monad, io} ||
      % the first action is HTTP GET request
      A <- get_ip(),

      % the second and third actions are nested chains of requests
      B <- usecase_a(),
      C <- usecase_b()
   ].

usecase_a() ->
   [{monad, io} ||
      % the use-case executes three HTTP GET requests 
      A <- get_ip(),
      B <- get_ip(),
      C <- get_ip(),

      % the result of last HTTP GET request is feed to HTTP POST request
      Y <- post(C)
   ].

usecase_b() ->
   [{monad, io} ||
      % the use-case executes three HTTP GET requests 
      A <- get_ip(),
      B <- get_ip(),
      C <- get_ip(),

      % the result of last HTTP GET request is fed to HTTP POST request
      Y <- post(C),

      % the result of previous HTTP POST request is fed again to HTTP POST request
      Z <- post(Y)
   ].


get_ip() ->
   [{monad, id} ||
      A0 <- scenario:new("urn:http:httpbin:get"),
      A1 <- scenario:method('GET', A0),
      A2 <- scenario:url("http://127.0.0.1:8888/ip", A1),
      A3 <- scenario:request([origin], A2)
   ].

post(Y) ->
   [{monad, id} ||
      A0 <- scenario:new("urn:http:httpbin:post"),
      A1 <- scenario:method('POST', A0),
      A2 <- scenario:url("http://127.0.0.1:8888/post", A1),
      A3 <- scenario:header("Content-Type", "text/plain", A2),
      A4 <- scenario:payload(Y, A3),
      A5 <- scenario:request(A4)
   ].

