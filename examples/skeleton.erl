%%
%% The mandatory header(s) for each scenario file:
%%  * `-module(...).` identity of module
%%  * `-compile({parse_transform, monad}).` enable monads  
-module(skeleton).
-compile({parse_transform, monad}).

%% 
%% The workload scenario consists of attributes and actions. 
%% Attributes are functions that returns a scalar values, 
%% Action returns pure IO-monadic computations. 
%% Actions and attributes are exported using `-export([...]).` 

%% Typhoon requires each scenario to defined attributes:
%%  * `title()` a human readable scenario name
%%  * `t()` time in milliseconds to execute workload
%%  * `n()` number of concurrent session globally spawned in the cluster
%%  * `urn()` list of requests identifiers produced by workload scenario
-export([title/0, t/0, n/0, urn/0]).

%% Scenario shall provide actions:
%%  * `init()` an optional computation to be executed once by scenario session. 
%%             the result of computation is feed to main entry point.
%%  * `run(_)` executes workload scenario, 
-export([init/0, run/1]).

%%%----------------------------------------------------------------------------   
%%%
%%% attributes
%%%
%%%----------------------------------------------------------------------------   

%% human readable scenario title
title() ->
   "Skeleton Workload Scenario".

%% time to execute workload in milliseconds
t() ->
   60000.

%% number of concurrent session to spawn in the cluster.
n() ->
   2.

%% identifiers of requests to visualize
urn() ->
   [
      "urn:http:zalando:api",
      "urn:http:zalando:articles"
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% actions
%%%
%%%----------------------------------------------------------------------------   

%%
%% init scenario configuration
init() ->
   do(['Mio' ||        %% sequence of requests to execute as IO-monadic computation
      _ <- request(),  %% execute HTTP request and discard results
      A <- request(),  %% execute HTTP request and assign response to variable A
      return(A)        %% it just takes a value A and puts it in a IO context.
   ]).

%%
%% execute scenario, the operation is repeated  until `t()` is expired. 
run(_Config) ->
   do(['Mio' ||
      _ <- request(),  %% execute HTTP request 
      A <- article(),  %% execute another type of HTTP request
      return(A)
   ]).


%%
%% create HTTP request using nested function call syntax
request() ->
   % 4. return IO-monad, it promise HTTP response 
   scenario:request(
      % 3. set request header 
      scenario:header("Accept-Language", "de-DE",   
         % 2. set destination url
         scenario:url("https://api.zalando.com/",   
            % 1. create new HTTP request
            scenario:new("urn:http:zalando:api")    
         )
      )
   ).

%%
%% create HTTP request using chained function call syntax
article() ->
   % 1. create new HTTP request
   A = scenario:new("urn:http:zalando:articles"),
   % 2. set destination url     
   B = scenario:url("https://api.zalando.com/articles", A), 
   % 3. set request header
   C = scenario:header("Accept-Language", "de-DE", B),
   % 4. return IO-monad that focus lens on JSON field in HTTP response 
   scenario:request([content, {uniform}, id], C). 

