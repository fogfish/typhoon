# typhoon load scenario

Typhoon uses pure functional expressions to define load scenario. These expressions are built with Erlang flavored syntax - it is valid Erlang code. However, it requires no compilation or build of native packages. Workload scenarios are deployed to Typhoon cluster as plain text blobs via REST API. The scenario development requires a basic understanding of functional programming concepts and knowledge of Erlang syntax:
* [Erlang language tutorial](http://learnyousomeerlang.com/starting-out-for-real)
* [Erlang modules tutorial](http://learnyousomeerlang.com/modules#what-are-modules)
* [Erlang expressions](http://erlang.org/doc/reference_manual/expressions.html)


## Make Workload Scenario

Each load scenario is valid Erlang module. The [skeleton scenario](../examples/skeleton.erl) is defined and explained below. See also the [advanced example](../examples/httpbin.erl).
```erlang
%% 
%% The mandatory header for each scenario file. The workload scenario MUST have a `-module(...).`
%% definition as first line of code. The best practice require module name to be equals to 
%% name of file. 
%%
-module(skeleton).
-compile({parse_transform, monad}).

%% 
%% 
%% The workload scenario consists of attributes and actions. The attribute is a function that
%% returns a scalar value, the action returns a pure IO-monadic computation.  Current version of
%% Typhoon requires three attributes `t()`, `n()` and `urn()` and entry point action, called 
%% `run()`. These functions shall be exported `-export([...]).` from the module.
%%
-export([t/0, n/0, urn/0, run/1]).

%%
%% scenario attributes - pure function returns scalar values.
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
%% scenario entry-point, IO-monads 
run(_) ->
   [{do, 'Mio'} ||       %% define sequence of requests to execute as IO monadic type
      _ <- request(),    %% execute HTTP request and discard results
      A <- request(),    %% execute HTTP request and assign response to variable A
      return(A)          %% it just takes a value A and puts it in a IO context. 
   ].

%%
%% The request is an opaque data structure, the developer uses scenario interface to
%% manipulate this data structure and reflect the required protocol request. 
%% The dot notation is best approach to configure the request, however it do not exists at
%% pure functional languages. Thus, identity monad is used to chain configuration actions 
%% over data structure
request() ->
   [{do, 'Mid'} ||
      A <- scenario:new("urn:http:example"),         %% create new request and set unique id
      B <- scenario:url("http://example.com/", A),   %% define end-point
      scenario:request(B)                            %% return request
   ].
```

### Validate scenario

The tool implement `lint` end-point. You can validate scenario syntax, compile it and execute against SUT. 

```
curl -v -XPOST http://192.168.99.100:8080/lint/example \
   -H 'Content-Type: application/erlang' \
   --data-binary @examples/skeleton.erl
```

### Deploy scenario

There is `scenario` end-point to `PUT`, `GET` or `DELETE` workload scenario that is uniquely identified by a key.

```
curl -v -XPOST http://192.168.99.100:8080/scenario/example \
   -H 'Content-Type: application/erlang' \
   --data-binary @examples/skeleton.erl
```

Use web-browser to execute scenario and observe results (http://192.168.99.100:8080/example).



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

The function returns number of concurrent session globally spawned in the cluster. The following example defines 100 concurrent session in the cluster.

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
   [{do, 'Mio'} ||
      A <- usecase_a(),
      B <- usecase_b(A),
      return(B)
   ].
```

### request

The request is pure functional data structure that defined protocol behavior. We are using identity monad to emulate dot-bind notation in Erlang. The request is evaluated into IO action. The request is defined using scenario interface. 

```erlang
http_post_req() ->
   [{do, 'Mid'} ||
      A0 <- scenario:new("urn:http:xxx:zzz"),
      A1 <- scenario:method('POST', A0),
      A2 <- scenario:url("http://127.0.0.1:8888/post", A1),
      A3 <- scenario:header("Content-Type", "text/plain", A2),
      A4 <- scenario:payload("example", A3),
      scenario:request(A4)
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


### join/1

Joins any terms to binary string, the term is either in-line scalar value or function call. The produced string is acceptable by any request functions defined in chapter above. 

```erlang
-spec join([_]) -> binary().

%% E.g.
<<"abc">> = scenario:join([a, b, c]).
<<"a12c">> = scenario:join([a, 12, c]).
<<"http://example.com/abc">> = scenario:join(["http://example.com/", "abc"]).
<<"a12c">> = scenario:join([a, get_12(), c]).
```


### uid/0

Generate globally unique lexicographically ordered identity (k-order number). The function is usable to generate url pointing to unique object.

```erlang
-spec uid() -> binary().

%% E.g.
<<"002d21a2005b8443c4b4c000">> = scenario:uid().
<<"http://example.com/002d21a2005b8443db61c000">> = scenario:join(["http://example.com/", scenario:uid()]).
```



### uniform/1

Generates uniformly distributed integer or term. The function has overloaded meaning that depends on input data type:
* Generates integer on interval `1 .. N` if input has type integer.
* Generates value from set if input is type of list
The return value is binary string usable for url or payload generation.

```erlang
-spec uniform(integer() | [_]) -> binary().

%% E.g.
<<"5">> = scenario:uniform(10).
<<"11">> = scenario:uniform([10, 11]).
```



### pareto/1

Generate random integer on interval `1 .. N` or term from the set using bounded Pareto distribution with parameter A. The function has overloaded meaning that depends on input data type:
* Generates integer on interval `1 .. N` if input has type integer.
* Generates value from set if input is type of list

```erlang
-spec pareto(float(), integer() | [_]) -> binary().

%% E.g.
<<"5">> = scenario:pareto(10).
<<"11">> = scenario:pareto(0.1, [10, 11, 12, 13, 14]).
```



### ascii/1

Generate random ASCII payload of given length, characters are uniformly distributed.

```erlang
-spec ascii(integer()) -> binary().

%% E.g.
<<"Ik1i8qrSX0">> = scenario:ascii(10).
```



### text/1

Generates random text alike combination of given length.

```erlang
-spec text(integer()) -> binary().

<<"bi mbo  g ">> = scenario:text(10).
```


### json/1

Convert tuple list into JSON object

```erlang
-spec json([{atom, _}]) -> binary().

<<"{\"a\":1,\"b\":\"Bwt2x\"}">> = scenario:json([{a, 1}, {b, scenario:ascii(5)}]).
```

