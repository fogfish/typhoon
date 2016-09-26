## Typhoon

[![Build Status](https://secure.travis-ci.org/zalando/typhoon.svg?branch=master)](http://travis-ci.org/zalando/typhoon)

Typhoon is a stress and load testing tool for distributed systems that simulates traffic from a test cluster toward a system-under-test (SUT) and visualizes infrastructure-, protocol- and application-related latencies. It provides an out-of-the-box, cross-platform solution for investigating protocols and microservice latencies, and is operable as a standalone application. For scalability and accuracy, its runtime environment is [Erlang](http://www.erlang.org/).  

### Key Features and Functionality

Typhoon uses [Cubism.js](https://bost.ocks.org/mike/cubism/intro/#0) to visualize latencies. The visualizations help you to make quick decisions on optimal software configuration, the number of servers and concurrent connections you need, and other short-term considerations. Long-term, they can inform how you develop and extend your data and service architecture, choose new technologies, etc.

Typhoon also:
- provides a [REST API](https://github.com/zalando/typhoon/blob/master/docs/restapi.yaml).
- uses pure functional expressions to define workload scenarios that don't require any compilation or native package builds; [read more here](https://github.com/zalando/typhoon/blob/master/docs/scenario.md).
- scales up to dozens of individual nodes hosted in any cloud environment.
- uses peer-to-peer clustering, based on consistent hashing, to assemble and orchestrate load toward SUT. It helps to deal with possible network failures, and provides high availability for synthetic load and telemetry collections.
- approximates network delay, round-trip time, TLS handshakes, Time to First Byte and Time to Meaningful Response.
- evaluates protocol overhead by approximating packet metrics.
- estimates application performance.
- validates system performance and scalability while spawning a huge number of concurrent sessions.

You can read more about Typhoon's features in [this blog post](https://tech.zalando.de/blog/end-to-end-latency-challenges-for-microservices/).

### Inspiration

Typhoon's [architecture](docs/arch.md) and design reflect the principles of incremental scalability, decentralization and  optimistic replication. It derives inspiration from related efforts driven by Nokia (latency analysis on cellular networks), Google (web protocol enhancement and evolution), and other companies working in the mobile app space.

### Getting Started

Typhoon supplies pre-built releases for Linux/x86_64, MacOS/10.10.x and Docker platforms. Instructions for using these binaries are on the [GitHub releases page](https://github.com/zalando/typhoon/releases).

Build the latest version of Typhoon from the `master` branch. The build process requires [Erlang/OTP](http://www.erlang.org/downloads) version 18.0 or later. All development, including new features and bug fixes, take place on the `master` branch using forking and pull requests as described in these [contribution guidelines](docs/contribution.md).

Install directions with AWS-related details are [here](https://github.com/zalando/typhoon/blob/master/docs/install.md).

### Running Typhoon

The easiest way to run a standalone instance is with the Docker container. The option is viable only if you've configured either [Docker Toolbox](https://www.docker.com/products/docker-toolbox) or [the Docker daemon](https://docs.docker.com/engine/reference/commandline/dockerd/). Use the latest [release version](https://github.com/zalando/typhoon/releases) instead of `x.y.z`:

```
docker run -it -p 8080:8080 registry.opensource.zalan.do/hunt/typhoon:x.y.z
```

This A) starts a single Typhoon node as a Docker container and B) exposes services using the REST API on port 8080. By default it is bound to following the IP address 192.168.99.100 on MacOS. If you're using a different platform, please check your Docker configuration.  

You can also spawn Typhoon using native platform binaries. See the installation instructions on the [GitHub releases page](https://github.com/zalando/typhoon/releases):
```
/usr/local/typhoon-x.y.z foreground
```

This brings Typhoon up and running. The application uses local IP address 127.0.0.1 and port 8080 to offer services. To double-check, and also make sure the REST API is exposed, use this command:  

```
curl http://192.168.99.100:8080/health/peer
```
The application should return list of cluster peers `["typhoon@172.17.0.2"]`.   

Next, define a simple workload scenario and publish it to Typhoon:
```
curl -XPUT http://192.168.99.100:8080/scenario/example \
   -H 'Content-Type: application/erlang' \
   --data-binary @examples/skeleton.erl
```  

Open the link `http://192.168.99.100:8080/example` in your web browser to manage the workload and analyze the results. The user interface should look similar to this:  

![User interface screenshot](screenshot.png)

Click the `run` button to kick off stress testing. Typhoon has a 60-second delay (approximately) before it renders the first result: network delay, roundtrip time, TLS handshake, Time to First Byte, and Time to Meaningful Response. It also evaluates protocol overhead at this time by approximating packet metrics, and estimates application performance.

Congrats! You have successfully started a Typhoon, written a stress test scenario, deployed it to a cluster and analyzed your system's behavior.

### Running a local Typhoon cluster 

Use docker containers to spawn three node cluster on local environment. The following examples shows how to spawn cluster seeder and other nodes. Use the latest [release version](https://github.com/zalando/typhoon/releases) instead of `x.y.z`

Let's spawn a seeder node, this node is used by other Typhoon peers to discover each other.
```
docker run -d -p 8080:8080 registry.opensource.zalan.do/hunt/typhoon:x.y.z
``` 

Next, the identity of seeder node is needed to spawn other peers. We can use health check api for this 
```
curl http://192.168.99.100:8080/health/peer
```
The application should return list of cluster peers `["typhoon@172.17.0.2"]`. This identity shall be passed to other containers within `EK_SEED` environment variable. The following example spawn more cluster peers: 
```
docker run -d -p 8080 -e "EK_SEED=typhoon@172.17.0.2" registry.opensource.zalan.do/hunt/typhoon:x.y.z
docker run -d -p 8080 -e "EK_SEED=typhoon@172.17.0.2" registry.opensource.zalan.do/hunt/typhoon:x.y.z
``` 

We can validate that all peers joins the cluster using health check api 
```
curl http://192.168.99.100:8080/health/peer
```
The application should return list of cluster peers 
`["typhoon@172.17.0.2","typhoon@172.17.0.3","typhoon@172.17.0.4"]`.   


### Next steps

* [Typhoon deployment](docs/install.md)
* [Understanding Typhoon scenario file](docs/scenario.md)

### Contributing/Bugs

Typhoon is Apache 2.0 licensed and accepts contributions via GitHub pull requests:

* Fork the repository on GitHub
* Read the README.md for build instructions

### commit message

The commit message helps us to write a good release note, speed-up review process. The message should address two question what changed and why. The project follows the template defined by chapter [Contributing to a Project](http://git-scm.com/book/ch5-2.html) of Git book.

>
> Short (50 chars or less) summary of changes
>
> More detailed explanatory text, if necessary. Wrap it to about 72 characters or so. In some contexts, the first line is treated as the subject of an email and the rest of the text as the body. The blank line separating the summary from the body is critical (unless you omit the body entirely); tools like rebase can get confused if you run the two together.
> 
> Further paragraphs come after blank lines.
> 
> Bullet points are okay, too
> 
> Typically a hyphen or asterisk is used for the bullet, preceded by a single space, with blank lines in between, but conventions vary here
>


### bugs
If you experience any issues with Typhoon, please let us know via [GitHub issues](https://github.com/zalando/typhoon/issue). We appreciate detailed and accurate reports that help us to identity and replicate the issue. 

* **Specify** the configuration of your environment. Include which operating system you use and the versions of runtime environments. 

* **Attach** logs, screenshots and exceptions, in possible.

* **Reveal** the steps you took to reproduce the problem.


### Contacts

* email: dmitry.kolesnikov@zalando.fi
* bugs: [here](https://github.com/zalando/typhoon/issues) 

### Changelog

Typhoon uses [semantic versions](http://semver.org) to identify stable releases. 
 
* [0.7.3](https://github.com/zalando/typhoon/releases/tag/0.7.3) - support local clustering
* [0.7.0](https://github.com/zalando/typhoon/releases/tag/0.7.0) - improve latency sampling
* [0.6.1](https://github.com/zalando/typhoon/releases/tag/0.6.1) - use pure functional expressions to define load scenarios
  

# License

Copyright 2015 Zalando SE

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
