# Typhoon Tips and Hints

## Post random text 

```erlang
post(_) ->
   do([m_http ||
      _ /= new("http://127.0.0.1:8888/post"),
      _ /= method('GET'),
      _ /= header("Connection", "keep-alive"),
      _ /= payload(scenario:text(1024)),
      _ /= request(),
      return(_)
   ]).
```

## Define report alias for multiple urls

Use a `label` socket option to define an alias for request

```erlang
es_req_put(Len) ->
   do([m_http ||
      _ /= new("http://127.0.0.1:8888/ip", [{label, "http://127.0.0.1:8888/"}]),
      _ /= x('PUT'),
      _ /= h("Connection: keep-alive"),
      _ /= d( ... ),
      _ /= r(),
      return(_)      
   ]).
```
