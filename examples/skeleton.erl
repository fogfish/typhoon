-module(skeleton).
-compile({parse_transform, monad}).

%% 
%% exported functions
-export([t/0, n/0, urn/0, run/0]).


%%
%% scenario attributes
%%

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
   
%%
%% scenario entry-point
%%
run() ->
   [{monad, io} ||
      A <- request()
   ].

request() ->
   [{monad, id} ||
      A <- scenario:new("urn:http:example"),
      B <- scenario:url("http://example.com/", A),
      C <- scenario:request(B)
   ].
