# typhoon

distributed system stress and load testing tool.

The tool simulates a traffic from test cluster towards system-under-test (SUT). The purposes of tool is validation of system performance and scalability, while spawning huge number of concurrent sessions.      

## Architecture

The tool is built from homogeneous nodes, each node is responsible to execute load scenarios and provides management interface. Nodes join the cluster and built a consistent hashing ring. The ring is used to store load scenario and collect the test results. The cluster uses Erlang distribution and add-on libraries to implement clustering. The traffic is generated on each node once scenario is executed.

The tool is is composed of multiple Erlang application:

* _typhoon_ - load simulator
* _aura_ - high-performance persistent queue to exchange sampled data
* _zephyrus_ - rest api and user interfaces 


## Build

The development requires Erlang/OTP environment 17.3 or later.
Please see following documentation on the development environment
* http://www.erlang.org/doc/ 
* http://www.erlang.org/doc/installation_guide/INSTALL.html

The project uses Makefile (see https://github.com/fogfish/makefile) to build project and assemble deployment packages.

## Configure

tbd

## Load scenario definition

The scenario is json file, see examples for elastic search and httpbin interface in apps/typhoon/priv.

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

Note: the given syntax construction is victim of code injection, thus access to the tool shall not be open to public networks.

## Usage

### define load scenario

```
curl -XPUT \
   http://localhost:8080/scenario/:id \
   -H 'Content-Type: application/json' \
   -d @myload.json
```

### remove load scenario

```
curl -XDELETE \
   http://localhost:8080/scenario/:id
```

### read load scenario

```
curl -XGET \
   http://localhost:8080/scenario/:id
```

### execute load scenario

```
curl -XGET \
   http://localhost:8080/run/:id
```

Use browser and open http://localhost:8080/:id to follow the progress


## Known limitations

* The current version is optimized for http(s) protocol only, support for other protocols and protocol plug-in interface is planned for future releases

* Scenario files are stored in-memory

* The access to the tool shall not be exposed to public networks.
 

# License

Copyright 2015 Zalando SE

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
