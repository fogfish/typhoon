## typhoon load scenario

The scenario is json file, see examples for [elastic search](apps/typhoon/priv/elastic.json) and [httpbin](apps/typhoon/priv/httpbin.json).

```
{
   "n":  1,  // number of load unit processes to spawn in cluster
   "t": 60,  // load process time-to-live, the process dies once time-out is exceeded
   "seq":    // sequence of protocol operations to generate the load
   [
      {
         // unique identifier of operation, the identity is used to persist KPI metrics and
         // generate reports. The identity MUST be valid URN instance
         "id"  : "urn:http:elastic:get",

         //
         // protocol method, request type
         "req" : "POST",

         //
         // destination url template
         "url" : "http://docker:9200/kv/test/{.typhoon:int(1000).}",

         //
         // set of protocol headers
         "head": {"Connection": "keep-alive"}

         //
         // request payload template
         "data": "{\"text\":\"{.typhoon:text(4096).}\"}"
      },
      ...
   ]
}
```

The content of ```url``` and ```data``` fields are enriched using template engine, the syntax construction ```{. .}``` is expanded by the result of corresponding function call. 

* ```{.typhoon:uid().}``` generate globally unique sequential identity
* ```{.typhoon:int(N).}``` generate uniformly distributed integer on interval 0..N
* ```{.typhoon:pareto(A,N).}``` generate random integer on interval 0..N using bounded Pareto distribution with parameter A.
* ```{.typhoon:ascii(N).}``` generate random ASCII payload of given length, characters are uniformly distributed.
* ```{.typhoon:text(N).}``` generate random text alike combination

The application uses white-list concept to protect environment from code injection. The list of allowed function calls is defined by `typhoon.hrl`.


