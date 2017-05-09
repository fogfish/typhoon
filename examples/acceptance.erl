-module(smoketest).
-compile({parse_transform, monad}).

-export([test_articles_api/1]).


test_articles_api(_) ->
   do([m_http_scenario ||
      _ /= 'Given'(),
      _ /= url("https://api.zalando.com/articles"),

      _ /= 'When'(),
      _ /= method('GET'),
      _ /= header("Accept-Language", "de-DE"),
      _ /= header("Connection", "close"),

      _ /= 'Then'(),
      _ /= match(status, 400),
      _ /= match(header, "Connection: close"),
      _ /= match(header, "Transfer-Encoding: chunked"),
      _ /= eq([page], 5),
      _ /= gt(['totalPages'], 10000),

      return(_)
   ]).