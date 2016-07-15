-module(skeleton).
-compile({parse_transform, monad}).

%%
%% scenario attributes
-export([title/0, t/0, n/0, urn/0]).

%%
%% scenario entry point
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
      "urn:http:example"
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% actions
%%%
%%%----------------------------------------------------------------------------   

%%
%% scenario configuration
%%
init() ->
   [{do, 'Mio'} ||
      A <- request(),
      return(A)
   ].

%%
%% scenario entry-point
%%
run(_Config) ->
   [{do, 'Mio'} ||
      A <- request(),
      return(A)
   ].

request() ->
   [{do, 'Mid'} ||
      A <- scenario:new("urn:http:example"),
      B <- scenario:url("http://example.com/", A),
      scenario:request(B)
   ].
