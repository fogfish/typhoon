# typhoon config

The tool uses Erlang [config file format](http://www.erlang.org/doc/man/config.html).


## node discovery

The list of cluster seed nodes is defined in `seed` option of `ambit` application.

```
{ambit, [
   ...
   {seed,       ['a@127.0.0.1']}
]}
```

## http

rest api port and backlog pool is defined in `restd` application

```
{restd, [
   {default, [
      {uri,  "http://*:8080"},
      {pool, 10}
      ...
   ]}
]}
```