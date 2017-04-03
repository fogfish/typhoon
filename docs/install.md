# Typhoon Installation

This application has been tested on Linux and Mac OS X. It should work on Erlang-supported platforms.

## Compilation

Typhoon supplies pre-built releases for Linux/x86_64, MacOS/10.10.x and Docker platforms. Instructions for using these binaries are on the [GitHub releases page](https://github.com/zalando/typhoon/releases). The compilation is only required to assemble a custom package.

```
git clone https://github.com/zalando/typhoon
cd typhoon
make
make test
make pkg
```

The result is bundle tar-ball archive as-is deployable to any host. It contains both a Erlang VM, all required dependencies and the Typhoon application `typhoon-{vsn}.{arch}.{plat}.bundle`.

It is possible to assemble cross-platform packages on MacOS, but this requires a Docker run-time.

```
make clean
make
make pkg PLAT=Linux 
```

## Development

Build and run typhoon in your development console:
```
make && make run
```

The command with boot Erlang virtual machine and opens shell. Now you are able to start typhoon is debug mode. You shall be able to use rest api and open typhoon dashboard in the browser once these subsystems are launched:

```erlang
%% start telemetry management subsystem
aura:start().

%% start typhoon core subsystem
typhoon:start().

%% start rest api subsystem
zephyrus:start().
```


## Installation

Copy the bundle archive to destination host and execute it:

```
sudo bash typhoon-x.y.z+x86_64.Linux.bundle
```

The command installs 
* the application binaries to `/usr/local/typhoon`
* the bootstrap script to `/etc/init.d/typhoon`


## Configuration

The tool uses Erlang's [config file format](http://www.erlang.org/doc/man/config.html).
```
/usr/local/typhoon/releases/x.y.z/sys.config
```

The user-configurable options and possible values are defined in the [configuration guidelines](../rel/sys.config).

## Running

```
/etc/init.d/typhoon start
/etc/init.d/typhoon stop
```

## Experimental: Deployment to Amazon

The application provides [CloudFormation](https://aws.amazon.com/cloudformation/) templates to automate cloud deployment:
* [rel/typhoon.aws](../rel/typhoon.aws) uses on-demand ec2 compute capacity 
* [rel/typhoon.spot](../rel/typhoon.spot) uses spot ec2 compute capacity

We have used YAML syntax to define these templates. You need to convert them to JSON prior to deployment, using the following utility:

```
curl -O -L https://github.com/bronze1man/yaml2json/raw/master/builds/darwin_amd64/yaml2json
```

Generate on-demand an EC2 compute-capacity CloudFCormation template:
```
yaml2json < rel/typhoon.aws  > typhoon.json
``` 

The output is a valid AWS CloudFormation JSON file. You can use AWS's Console/Cloud Formation service to deploy the stack to the cloud. The automated deployment procedure is optimized from [Amazon Linux distribution](https://aws.amazon.com/amazon-linux-ami). You need to parameterize your stack with:
* desired AMI
* deployment subnet
* SSH key
