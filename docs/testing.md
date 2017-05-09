# typhoon acceptance testing

The purpose of acceptance testing is to validate SUT compliance with the business requirements of microservices. Typhoon uses a pure functional notation to script business cases and support Behavior Driven Development (Given/When/Then). The acceptance scenario are built with Erlang flavored syntax - it is valid Erlang code, in similar style as [stress testing](scenario.md). Technically, this is a developer friendly syntax sugar to check correctness of SUT.  

The Given/When/Then connects cause-and-effect to the microservices concept of input/process/output:

**Given** identity the business context and known state for the use-case.

**When** defines key actions for the interaction with service.

**Then** observes output of microservices and validate its correctness. 

Typhoon does not implement a natural language test definition like traditional BDD tools. The syntax uses pure function approach to make rough approximation of this steps into world of microservices.

## Example

```erlang
-module(hello_world).
-compile({parse_transform, monad}).
-compile(export_all).

create(_) ->
   do([m_http_scenario ||
      _ /= 'Given'(),
      _ /= url("http://example.com/resource"),
      _ /= payload("{\"name\": \"resource\"}"),

      _ /= 'When'(),
      _ /= method('POST'),
      _ /= header("Content-Type: text/plain"),
      _ /= header("Connection", "close"),

      _ /= 'Then'(),
      _ /= match(status, 201),
      _ /= match(header, "Transfer-Encoding: chunked"),
      _ /= has(id),
      _ /= eq(name, "resource"),

      _ /= return(_)
   ]).
```


## Syntax Guide

Typhoon acceptance test scripts is an Erlang module. The module implements a BDD coverage for *Feature*, the exported functions are *Scenario* in do-notation.

```erlang
%% declare feature
-module(feature_id).
-compile({parse_transform, monad}).

%% declare visibility of feature *Scenario* 
-export([scenario/1, other/2]).

scenario(_) ->
   %% specification for this scenario

other(_) ->
   %% specification for other scenario

```

### do-notation 

The acceptance scenario is valid Erlang function of arity 1 `fun( (_) -> _ )`. This function declares scenario behavior using "do"-notation. The "do"-notation, so called monadic binding form, is well know in functional programming languages such as [Haskell](https://en.wikibooks.org/wiki/Haskell/do_notation), [Scala](http://docs.scala-lang.org/tutorials/tour/sequence-comprehensions.html) and [Erlang](https://github.com/fogfish/datum/blob/master/doc/monad.md). The workload scenario is a collection of nested `do-notation` in context of a [state monad](https://acm.wustl.edu/functional/state-monad.php).

```erlang
scenario(_) ->
   do([m_http_scenario ||
      _ /= 'Given'(),
      %% identity the business context and known state for the use-case.

      _ /= 'When'(),
      %% defines key actions for the interaction with service.

      _ /= 'Then'()
      %% observes output of microservices and validate its correctness. 

      _ /= return(_)
   ]).
```

### Given

The clause defines the context for acceptance scenario. It defines a mandatory `url` and an optional `payload`.

```erlang
   _ /= 'Given'(),
   _ /= url("http://example.com/resource"),

   %% Optional
   _ /= payload("..."),
```

### When

The clause defines actions, specify protocol behavior. It defined HTTP protocol `method` and `header`.

```erlang
   _ /= 'When'(),
   _ /= method('GET'),
   _ /= header("Content-Type: text/plain"),
   _ /= header("Connection", "close"),
```

### Then

The clause captures the output of HTTP request and observes its correctness through set of assertion methods.

```erlang
   _ /= 'Then'(),

   %% Match HTTP protocol conditions to desired value
   _ /= match(status, ...),
   _ /= match(header, ...),   

   %% Check existence of attribute in response
   _ /= has(...)

   %% Compare attribute to specified value
   _ /= eq(..., ...),
   _ /= ne(..., ...),
   _ /= le(..., ...),
   _ /= lt(..., ...),
   _ /= ge(..., ...),
   _ /= gt(..., ...),
```

The scenario decodes content into suitable format using Content-Type of HTTP response as hint. However, the current version is JSON centric. It parses JSON to nested set of key-value pairs and offers *lenses* to focus on particular value. 

#### match

The function match a protocol attributes in the response:
 * match HTTP `status` code to specified value
 * match HTTP `header` to specified value

```erlang
   _ /= match(status, 201),
   _ /= match(header, "Transfer-Encoding: chunked"),
```

#### has

The function take a *lens* and observe if the focused value exists in response JSON object.

```erlang
   %% the JSON response has `resources` attribute 
   _ /= has([resources]),

   %% the JSON response has list of `resources`, the list is not empty
   _ /= has([resources, 1]),

   %% the JSON response has list of `resources`, the first element of list has attribute volume
   _ /= has([resources, 1, volume]),
``` 

#### compare

There is a collection of compare functions that take a *lens* and observe if the focused value matches the declared value:
* `eq` equal
* `ne` not equal
* `le` less
* `lt` less then
* `ge` greater
* `gt` greater then  

```erlang
   %% the JSON response has list of `resources`, the first element of list has attribute volume

   %% the volume equal to 1000
   _ /= eq([resources, 1, volume], 1000),

   %% the volume greater 1000
   _ /= gt([resources, 1, volume], 1000),
```

#### lenses

> Lenses, also known as functional references, are a powerful way of looking at, constructing, and using functions on complex data types.

The tool expresses concept of lens as list of key to focus on nested Json object. Each element of list identify a next entity to focus. The tool implement following built-in lenses, which are available for `has` and `compare` functions:
* `attribute` focuses on attribute value pair of JSON object 
* `index` focuses on nth element of list value, the enumeration is started with first (1) element

```erlang
   [resources]
   [resources, 1]
   [resources, 1, volume]
```


## Acceptance test management

Typhoon provides REST API for execution of acceptance tests. 

Spawn an instance of the tool 
```
docker run -it --name typhoon --rm -p 8080:8080 registry.opensource.zalan.do/hunt/typhoon:latest
```

Execute a feature acceptance testing
```
curl -o example.html \
   http://localhost:8080/tests/example \
   -H 'Content-Type: application/erlang' \
   --data-binary @examples/acceptance.erl 
```

Open example.html with web-browser to see tests results

