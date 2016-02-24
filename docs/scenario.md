# typhoon load scenario

The load scenario is a JSON document. The document consists of common definitions, they are applicable to each request and sequence of individual actions performed by the tool. 



## common definition

### `@context`
it defines the schema and version of load scenario specification. It must be equal to `http://github.com/zalando/typhoon/schema/scenario/1.0`

### `n`
number of concurrent sessions to run

### `t`
duration for each session in milliseconds. the session is aborted once the timeout is expired

### `url`
optional base url, used to construct full request url.

### `header`
optional common set of headers attached to each request.

### `seq`
list of requests



## request definition


### `@id`
unique identity of the operation, its format is urn (e.g. `urn:http:httpbin.org`)

### `@type`
type of the operation, the following types are supported
* `http` - the action is http request
* `thinktime` - the action is client think time

### `method`
one of the following http methods: `GET`, `POST`, `PUT` and `DELETE`

### `url`
either full or relative url to send a request

### `header`
set of request specific http headers.

### `payload`
the application payload transmitted within the request.

### `t`
think time in milliseconds to suspend the sessions


## scripting and templates

The content of `url`, `header` and `payload` fields are enriched using template engine, the operator `{. .}` is expanded by the result of corresponding function call. 

* `{.scenario:uid().}` generate globally unique sequential identity
* `{.scenario:int(N).}` generate uniformly distributed integer on interval `0 .. N`
* `{.scenario:pareto(A,N).}` generate random integer on interval `0 .. N` using bounded Pareto distribution with parameter A.
* `{.typhoon:ascii(N).}` generate random ASCII payload of given length, characters are uniformly distributed.
* `{.typhoon:text(N).}` generate random text alike combination

The application uses white-list concept to protect environment from code injection. The list of allowed function calls is defined by scenario [interface](apps/scenario/src/scenarion.erl).



## example

See full example towards [httpbin](apps/typhoon/priv/httpbin.json).
```
{
   "@context": "http://github.com/zalando/typhoon/schema/scenario/1.0",
   "n": 10,
   "t": 60000,
   "url": "http://localhost:8888",
   "header": {"Connection": "keep-alive"},
   "seq": 
   [
      {
         "@id"     : "urn:http:httpbin.org",
         "method"  : "GET",
         "url"     : "/"
      },

      {
         "@id"     : "urn:thinktime:1",
         "@type"   : "thinktime",
         "t"       : 500
      },

      ...
   ]
}
```


