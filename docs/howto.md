# Typhoon Tips and Hints

## Post random text 

```erlang
post(_) ->
   [{do, 'Mid'} ||
      A <- scenario:new("urn:http:httpbin:post"),
      B <- scenario:method('POST', A),
      C <- scenario:url("http://127.0.0.1:8888/post", B),
      D <- scenario:header("Content-Type", "text/plain", C),
      E <- scenario:payload(scenario:text(1024), D),
      scenario:request(E)
   ].
```

