# typhoon load scenario

Typhoon uses pure functional expressions to define load scenario. These expressions are built with Erlang flavored syntax - it is valid Erlang code. However, it requires no compilation or build of native packages. Workload scenarios are deployed to Typhoon cluster as plain text blobs via REST API. The scenario development requires a basic understanding of functional programming concepts and knowledge of Erlang syntax:
* [Erlang language tutorial](http://learnyousomeerlang.com/starting-out-for-real)
* [Erlang modules tutorial](http://learnyousomeerlang.com/modules#what-are-modules)
* [Erlang expressions](http://erlang.org/doc/reference_manual/expressions.html)


## do-notation

The "do"-notation, so called monadic binding form, is well know in functional programming languages such as [Haskell](https://en.wikibooks.org/wiki/Haskell/do_notation), [Scala](http://docs.scala-lang.org/tutorials/tour/sequence-comprehensions.html) and [Erlang](https://github.com/fogfish/datum/blob/master/doc/monad.md). The workload scenario is a collection of nested `do-notation` in context of a [state monad](https://acm.wustl.edu/functional/state-monad.php). 

The workload scenario defines two computations:
* `fun init/0` is an optional computation to be executed once by scenario session. the result of computation is feed to main entry point.
* `fun run/1` executes workload scenario

```erlang
init() ->
   do([m_state ||
      ...
      return(_)
   ]).

run(_) ->
   do([m_state ||
      ...
      return(_)
   ]).
``` 

The protocol operations (IO actions) are defined using do-notation in context of protocol. The protocol operations are defined using monad utility operations (see `/=` operand).
```
request() ->
   do([m_http ||
      ...
      return(_)
   ]).
```

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
%% The workload scenario consists of attributes and actions. 
%% Attributes are functions that returns a scalar values, 
%% Action returns pure IO-monadic computations. 
%% Actions and attributes are exported using `-export([...]).` 

%% Typhoon requires each scenario to defined attributes:
%%  * `title()` a human readable scenario name
%%  * `t()` time in milliseconds to execute workload
%%  * `n()` number of concurrent session globally spawned in the cluster
-export([title/0, t/0, n/0]).

%% Scenario shall provide actions:
%%  * `init()` an optional computation to be executed once by scenario session. 
%%             the result of computation is feed to main entry point.
%%  * `run(_)` executes workload scenario, 
-export([run/1]).

%%
%% scenario attributes - pure function returns scalar values.
%%

%% human readable scenario title
title() ->
   "Skeleton Workload Scenario".

%% time to execute workload in milliseconds
t() ->
   60000.

%% number of concurrent session to spawn in the cluster.
n() ->
   1.
   
%%
%% scenario entry-point, IO-monads 
run(_) ->
   do([m_state ||        %% define sequence of requests to execute as IO monadic type
      _ <- request(),    %% execute HTTP request and discard results
      A <- request(),    %% execute HTTP request and assign response to variable A
      return(A)          %% it just takes a value A and puts it in a IO context. 
   ]).

%%
%% The request is an opaque data structure, the developer uses scenario interface to
%% manipulate this data structure and reflect the required protocol request. 
%% The dot notation is best approach to configure the request, however it do not exists at
%% pure functional languages. Thus, identity monad is used to chain configuration actions 
%% over data structure
request() ->
   do([m_http ||
      _ /= new("http://example.com/"),
      _ /= method('GET'),
      _ /= request(),
      return(_)
   ]).
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

Use web-browser to execute scenario and observe results (http://localhost:8080/example).



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


## Scenario actions

Actions are composition of functions, each function builds and executes protocol operation (e.g. HTTP request), the result of protocol operation is returned to next function and so on. We are using IO-monad to isolate side-effect and protocol stack from scenario developers.

Let's us consider following example. The scenario is build around two use-case A and B. A executes IO operation and returns result to B.
```erlang
run() -> 
   do([m_state ||
      _ <- usecase_a(),
      _ <- usecase_b(_),
      return(_)
   ].
```

### request

The request is pure functional data structure that defined protocol behavior. The request is evaluated into IO action.  

```erlang
http_post_req() ->
   do([m_http ||
      _ /= new("http://127.0.0.1:8888/post"),
      _ /= method('POST'),
      _ /= header("Content-Type", "text/plain"),
      _ /= payload("example"),
      _ /= request()
      return(_)
   ]).
```

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

