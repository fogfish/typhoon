# typhoon load scenario

Typhoon uses pure functional expressions to define load scenario. These expressions are built with Erlang flavored syntax - it is valid Erlang code. However, it requires no compilation or build of native packages. Workload scenarios are deployed to Typhoon cluster as plain text blobs via REST API. The scenario development requires a basic understanding of functional programming concepts and knowledge of Erlang syntax:
* [Erlang language tutorial](http://learnyousomeerlang.com/starting-out-for-real)
* [Erlang modules tutorial](http://learnyousomeerlang.com/modules#what-are-modules)
* [Erlang expressions](http://erlang.org/doc/reference_manual/expressions.html)


Each load scenario is valid Erlang module. The [skeleton scenario](../examples/skeleton.erl) is defined and explained below. See also the [advanced example](../examples/httpbin.erl).
```erlang
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
   1.

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
```

The workload scenario MUST have a `-module(...).` definition as first line of code. The best practice require module name to be equals to name of file. 


The workload scenario consists of attributes and actions. The attribute is a function that returns a scalar value, the action returns a pure IO-monadic computation. Current version of Typhoon requires three attributes `t()`, `n()` and `urn()` and entry point action, called `run()`. These functions shall be exported `-export([...]).` from the module.


## Scenario attributes


### duration

`-spec t() -> integer().`

The function returns the duration of each sessions in milliseconds. The workload session is aborted when the timeout is expired. The following example defines 60 second execution time for workload. 

```erlang
t() ->
   60000.
```


### concurrency

`-spec n() -> integer().`

The function returns number of concurrent session to globally spawn in the cluster. The following example defines 100 concurrent session in the cluster.

```erlang
n() ->
   100.
```  


### identity

`-spec urn() -> [string()].`

The function returns list of requests identifiers produced by workload scenario. These identifiers are used by front-end component to visualize and report latencies. The following example specify a single request identified by `"urn:http:example"` token.

```erlang
urn() ->
   [
      "urn:http:example"
   ].
```



## Scenario actions

Actions are composition of functions, each function builds and executes protocol operation (e.g. HTTP request), the result of protocol operation is returned to next function and so on. We are using IO-monad to isolate side-effect and protocol stack from scenario developers.

Let's us consider following example. The scenario is build around two use-case A and B. A executes IO operation and returns result to B.
```erlang
run() -> 
   [{monad, io} ||
      A <- usecase_a(),
      B <- usecase_b(A)
   ].
```

### request

The request is pure functional data structure that defined protocol behavior. We are using identity monad to emulate dot-bind notation in Erlang. The request is evaluated into IO action. The request is defined using scenario interface. 

```erlang
http_post_req() ->
   [{monad, id} ||
      A0 <- scenario:new("urn:http:xxx:zzz"),
      A1 <- scenario:method('POST', A0),
      A2 <- scenario:url("http://127.0.0.1:8888/post", A1),
      A3 <- scenario:header("Content-Type", "text/plain", A2),
      A4 <- scenario:payload("example", A3),
      A5 <- scenario:request(A4)
   ].
```


## Scenario interface


### new

```erlang
-spec new(string()) -> _.
```

The function defines new request, it requires a unique request identity that is built after urn syntax, urn schema defines communication protocol. The request data structure is opaque to scenario developer. It shall be used with other scenario methods


### method

```erlang
-spec method(atom() | string() | binary(), _) -> _.
```

The function defines method to request (default value is `'GET'`). One of the following HTTP methods are allowed `'GET'`, `'POST'`, `'PUT'` and `'DELETE'`.


### url

```erlang
-spec url(string() | binary(), _) -> _.
```

The function defines absolute url to request resource


### header

```erlang
-spec header(string(), string(), _) -> _.
```

The function set HTTP header to the request. The first argument is header name, second is header value.


### payload

```erlang
-spec payload(string() | binary(), _) -> _.
```

The function set HTTP payload.


### request

```erlang
-spec request(_) -> _.
-spec request([atom()], _) -> _.
```

The function return IO computation corresponding to the defined request. 


### thinktime

```erlang
-spec thinktime(_, integer()) -> _.
```

The function return IO computation that emulates think-time of terminal.


## Scenario utility

### uid

```erlang
-spec uid() -> binary().
```

generate globally unique sequential identity


### int

```erlang
-spec int(integer()) -> binary().
```

generate uniformly distributed integer on interval `1 .. N`


### pareto

```erlang
-spec pareto(float(), integer()) -> binary().
```

generate random integer on interval `1 .. N` using bounded Pareto distribution with parameter A.


### ascii

```erlang
-spec ascii(integer()) -> binary().
```

generate random ASCII payload of given length, characters are uniformly distributed.


### text

```erlang
-spec text(integer()) -> binary().
```

generate random text alike combination



