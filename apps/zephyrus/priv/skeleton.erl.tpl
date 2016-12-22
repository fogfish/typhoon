%%
%% The mandatory header(s) for each scenario file:
%%  * `-module(...).` identity of module
%%  * `-compile({parse_transform, monad}).` enable monads
-module(${MODULE_NAME_PLACEHOLDER}).
-compile({parse_transform, monad}).

%% 
%% The workload scenario consists of attributes and actions 
%% Attributes are functions that return scalar values 
%% Action returns pure IO-monadic computations. 
%% Actions and attributes are exported using `-export([...]).`

%% Typhoon requires each scenario to define attributes:
%%  * `title()` a human readable scenario name
%%  * `t()` time in milliseconds to execute workload
%%  * `n()` number of concurrent sessions globally spawned in the cluster
%%  * `urn()` list of request identifiers produced by workload scenario
-export([title/0, t/0, n/0, urn/0]).

%% Scenario shall provide actions:
%%  * `init()` an optional computation to be executed once by scenario session 
%%             the result of computation is fed to main entry point
%%  * `run(_)` executes workload scenario 
-export([init/0, run/1]).

%%%----------------------------------------------------------------------------   
%%%
%%% attributes
%%%
%%%----------------------------------------------------------------------------   

%% human readable scenario title
title() ->
   "${TITLE_PLACEHOLDER}".

%% time to execute workload in milliseconds
t() ->
   60000.

%% number of concurrent sessions to spawn in the cluster.
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
   do([m_state ||        %% sequence of requests to execute as IO-monadic computation
      _ <- request(),  %% execute HTTP request and discard results
      A <- request(),  %% execute HTTP request and assign response to variable A
      return(A)        %% it just takes a value A and puts it in an IO context.
   ]).

%%
%% execute scenario, the operation is repeated  until `t()` is expired. 
run(_Config) ->
   do([m_state ||
      _ <- request(),  %% execute HTTP request 
      A <- article(),  %% execute another type of HTTP request
      return(A)
   ]).


%%
%% create HTTP request using nested function call syntax
request() ->
   do([m_http ||
      % 1. create new HTTP request
      _ /= new("urn:http:zalando:api"),
      
      % 2. set destination url
      _ /= url("https://api.zalando.com/"),

      % 3. set request header 
      _ /= header("Accept-Language", "de-DE"),
      _ /= header("Connection", "close"),

      % 4. build HTTP promise
      _ /= get(),

      %% 5. return results
      return(_)
   ]).

%%
%% create HTTP request using chained function call syntax
article() ->
   do([m_http ||
      % 1. create new HTTP request
      _ /= new("urn:http:zalando:articles"),
      
      % 2. set destination url     
      _ /= url("https://api.zalando.com/articles"), 

      % 3. set request header
      _ /= header("Accept-Language", "de-DE"),
      _ /= header("Connection", "close"),

      % 4. build HTTP promise and decode result
      _ /= get(),
      _ /= decode(_),

      %% 5. return results
      return( scenario:lens([content, {uniform}, id], _) )
   ]).

