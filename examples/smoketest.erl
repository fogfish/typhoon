-module(smoketest).
-compile({parse_transform, monad}).

-export([test_articles_api/1]).


test_articles_api(_) ->
   do([m_smoke_it ||
      _ /= new("https://api.zalando.com/articles"),
      _ /= method('GET'),
      _ /= header("Accept-Language", "de-DE"),
      _ /= header("Connection", "close"),
      _ /= request(),

      _ /= check(status, 400),
      _ /= check(header, "Connection: close"),
      _ /= check(header, "Transfer-Encoding: chunked"),
      _ /= eq([page], 5),
      _ /= gt(['totalPages'], 10000),

      return(_)
   ]).