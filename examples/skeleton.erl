%%
%% The mandatory header(s) for each scenario file:
%%  * `-module(...).` identity of module
%%  * `-compile({parse_transform, monad}).` enable monads
-module(skeleton).
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
-export([title/0, t/0, n/0]).

%% Scenario shall provide actions:
%%  * `init()` an optional computation to be executed once by scenario session 
%%             the result of computation is fed to main entry point
%%  * `run(_)` executes workload scenario 
% -export([init/0, run/1]).
-export([run/1]).

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

%% number of concurrent sessions to spawn in the cluster.
n() ->
   2.

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
      _ /= new("https://api.zalando.com/"),
      _ /= method('GET'), 

      % 3. set request header 
      _ /= header("Accept-Language", "de-DE"),
      _ /= header("Connection", "close"),

      % 4. build HTTP promise
      _ /= request(),

      %% 5. return results
      return(_)
   ]).

%%
%% create HTTP request using chained function call syntax
article() ->
   do([m_http ||
      % 1. create new HTTP request 
      _ /= new("https://api.zalando.com/articles"),
      _ /= method('GET'),
      

      % 3. set request header
      _ /= header("Accept-Language", "de-DE"),
      _ /= header("Connection", "close"),

      % 4. build HTTP promise and decode result
      _ /= request(),

      % 5. parse and return results
      _ =< scenario:decode(_),
      return( scenario:lens([content, {uniform}, id], _) )
   ]).

