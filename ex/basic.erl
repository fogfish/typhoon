-module(basic).

-export([
   http_get/0
   % http_error/0
]).

http_get() ->
   [m_http ||
      _ > "GET http://httpbin.org/ip",
      _ > "Connection: close",

      _ < 200
   ].

% http_error() ->
%    [m_http ||
%       _ > "GET http://httpbin.org/ip",
%       _ > "Connection: close",

%       _ < 201
%    ].

