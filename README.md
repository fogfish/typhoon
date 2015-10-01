# typhoon

distributed system stress and load testing tool.

The tool simulates a traffic from test cluster towards system-under-test (SUT). The purposes of tool is validation of system performance and scalability, while spawning huge number of concurrent sessions.      

## architecture

[See specification](docs/arch.md)

## build

The project requires Erlang/OTP development environment [here](docs/erlang.md) and essential development tool sets. The project provides binary files for Linux x86_64 and MacOS 10.10.x platforms.

Use following commands to compile and make distributable package for your platform. The output is self-deployable bundle package ```typhoon-{vsn}+{head}.{arch}.{plat}.bundle```

```
   make
   make pkg
```

It is possible to assemble cross platform packages on MacOS. However it requires docker tool kit and docker image with Erlang/OTP environment. 

```
   make
   make pkg PLAT=Linux 
```


## configuration

[See config file](docs/config.md)


## usage

The tool provides rest api to manage load and stress test scenarios using curl command line
utility and implement html interface to visualize measurements. The load scenario is [json file](docs/unit.md)

Use browser to inspect the progress of the executed test ```http://localhost:8080/:id``` once test specification is defined to the cluster.

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
   http://localhost:8080/scenario/:id/spawn
```

## Known issues

* The current version is optimized for http(s) protocol only, support for other protocols and protocol plug-in interface is planned for future releases

* Scenario files are stored in-memory. However cluster manages 3 replicas.

* The access to the tool shall not be exposed to public networks.
 

# License

Copyright 2015 Zalando SE

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
