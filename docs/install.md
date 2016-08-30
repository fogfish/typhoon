# Typhoon installation

The application has been tested on Linux and Mac OS X. It should work on Erlang supported platforms.

## Compilation

Typhoon supplies pre-built releases for Linux/x86_64, MacOS/10.10.x and Docker platforms. Instructions for using these binaries are on the [GitHub releases page](https://github.com/zalando/typhoon/releases). The compilation is only required to assemble a custom package

```
git clone https://github.com/zalando/typhoon
cd typhoon
make && make pkg
```

The result is bundle tar-ball archive as-is deployable to any host. It contains both Erlang VM, all required dependencies and Typhoon application `typhoon-{vsn}.{arch}.{plat}.bundle`.

It is possible to assemble cross-platform packages on MacOS but this requires a docker run-time. 
```
make && make pkg PLAT=Linux 
```


## Installation

Copy the bundle archive to destination host and execute it
```
sudo bash typhoon-x.y.z+x86_64.Linux.bundle
```

The command installs 
* the application binaries to `/usr/local/typhoon`
* the bootstrap script to `/etc/init.d/typhoon`


## Configuration

The tool uses Erlang [config file format](http://www.erlang.org/doc/man/config.html).
```
/usr/local/typhoon/releases/x.y.z/sys.config
```

The user configurable options and possible values are defined at [configuration guideline](config.md).


## Running

```
/etc/init.d/typhoon start
/etc/init.d/typhoon stop
```


## Experimental: Deployment to Amazon

The application provides [cloud formation](https://aws.amazon.com/cloudformation/) templates to automate the cloud deployment.
* [rel/typhoon.aws](../rel/typhoon.aws) uses on-demand ec2 compute capacity 
* [rel/typhoon.spot](../rel/typhoon.spot) uses spot ec2 compute capacity

We have used YAML syntax to define these template. You need to convert them to json prior deployment using following utility:
```
curl -O -L https://github.com/bronze1man/yaml2json/raw/master/builds/darwin_amd64/yaml2json
```

Generate on-demand ec2 compute capacity cloud formation template
```
yaml2json < rel/typhoon.aws  > typhoon.json
``` 

Use AWS Console / Cloud Formation Service to deploy the service on your AWS account. 




