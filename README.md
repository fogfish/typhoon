## Typhoon

[![Build Status](https://secure.travis-ci.org/zalando/typhoon.svg?branch=master)](http://travis-ci.org/zalando/typhoon)

Typhoon is a stress and load testing tool for distributed systems that simulates traffic from a test cluster toward a system-under-test (SUT). For scalability and accuracy, its runtime environment Erlang-based, and it uses pure functional expressions to define load scenarios. Typhoon is operable as a standalone application and scales up to dozens of individual nodes hosted in any cloud environment.

### Key Features and Functionality

Typhoon:
- Provides an out-of-the-box, cross-platform solution for investigating microservice protocols and latencies
- Offers a REST interface for building, defining and spawning workload scenarios
- Collects telemetry and visualizes the results
- Approximates network delay, round-trip time, TLS handshakes, Time to First Byte and Time to Meaningful Response
- Evaluates protocol overhead by approximating packet metrics
- Estimates application performance
- Validates system performance and scalability while spawning a huge number of concurrent sessions

### Inspiration

Typhoon's architecture is defined by the principles of incremental scalability and decentralization. It derives inspiration from related efforts driven by Nokia (latency analysis on cellular networks), Google (web protocol enhancement and evolution), and other companies working in the mobile app space.

## Getting Started

### Getting Typhoon

The project supplies pre-build release for Linux/x86_64, MacOS/10.10.x and Docker platforms. Instructions for using these binaries are on the [GitHub releases page](https://github.com/zalando/typhoon/releases).

The latest version of `typhoon` can be build from `master` branch. The build process requires [Erlang/OTP](http://www.erlang.org/downloads) version 18.0 or later. All development, including new features and bug fixes, take place on `master` branch using forking and pull requests as described in [contribution guideline](docs/contribution.md).


### Running Typhoon

The Docker container is easiest way to run standalone instance of the application. The option is viable only if either [docker toolbox](https://www.docker.com/products/docker-toolbox) or docker daemon is configured at your environment. Use latest release version instead of `x.y.z`
```
docker run -it -p 8080:8080 registry.opensource.zalan.do/hunt/typhoon:x.y.z
```

This will start single typhoon node as docker container. It exposes services using rest api on port 8080 (by default it is bound to following ip address 192.168.99.100 on MacOS, please check your docker configuration on other platforms).  

It is possible to spawn the tool using native platform binaries, see the installation instructions on the [GitHub releases page](https://github.com/zalando/typhoon/releases). 
```
/usr/local/typhoon-x.y.z foreground
```

This bring typhoon up and running. The application uses local ip 127.0.0.1 and port 8080 to offer services.

Use following command to check if the application is up and running and the rest api is exposed. The application should return list of cluster peers `["typhoon@127.0.0.1"]`.   
```
curl http://192.168.99.100:8080/health/peer
```

Next define a simple workload scenario, publish it to typhoon.
```
curl -XPUT http://192.168.99.100:8080/scenario/example \
   -H 'Content-Type: application/erlang' \
   --data-binary @examples/skeleton.erl
```  

Open the link `http://192.168.99.100:8080/example` in web browser to manage the workload and analyze the results. User interface should look similar to following screen shot. Click `run` button to kick-off stress testing. The tool has about 60 second delay before it renders the first result. The tool renders Network delay, Round Trip Time, TLS Handshake, Time-to-First-Byte and Time-to-Meaningful-Response; evaluates protocol overhead by approximating packet metrics and estimates application performance.
![User interface screenshot](screenshot.png)

You have successfully started an typhoon, written a stress test scenario, deployed it to cluster and analyze the system behavior.

### Continue to...

* specification of [workload scenario](docs/scenario.md)
* explore [rest interface](docs/restapi.yaml)
* read [hints and code snippets](docs/howto.md) of workload scenarios 



## project details

### architecture
[See specification](docs/arch.md)



## contributing
See [contribution guideline](docs/contribution.md) for details on PR submission.



## bugs
See [bug reporting](docs/bugs.md) for guidelines on raising issues. 



## contacts

* email: dmitry.kolesnikov@zalando.fi
* bugs: [here](https://github.com/zalando/typhoon/issues) 

<!-- 

## build

The project requires Erlang/OTP development environment [check here instructions](docs/erlang.md) and essential development tool sets such as `git`, `make`, etc. The project provides binary files for Linux x86_64 and MacOS 10.10.x platforms (see releases).

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

![User interface screenshot](screenshot.png)

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

## known issues

* The current version is optimized for http(s) protocol only, support for other protocols and protocol plug-in interface is planned for future releases

* Scenario files are stored in-memory. However cluster manages 3 replicas.
 -->

# License

Copyright 2015 Zalando SE

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
