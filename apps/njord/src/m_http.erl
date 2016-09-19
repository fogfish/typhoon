%%
%%   Copyright 2016 Zalando SE
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @doc
%%   http IO monad
-module(m_http).

-export([return/1, fail/1, '>>='/2]).
-export([new/0, new/1, method/1, url/1, header/2]).
-export([request/1, request/0, thinktime/1]).

%%%----------------------------------------------------------------------------   
%%%
%%% state monad
%%%
%%%----------------------------------------------------------------------------   

return(X) ->
   fun(State) -> [X|maps:remove(fd, State)] end.

fail(X) ->
   m_state:fail(X).

'>>='(X, Fun) ->
   m_state:'>>='(X, Fun).

%%%----------------------------------------------------------------------------   
%%%
%%% http monad
%%%
%%%----------------------------------------------------------------------------   

%%
%% http request specification
-define(HTTP, {'GET', undefined, []}).

id()      -> lens:c([lens:map(fd,  {none, ?HTTP}), lens:t1()]).
method()  -> lens:c([lens:map(fd), lens:t2(), lens:t1()]).
url()     -> lens:c([lens:map(fd), lens:t2(), lens:t2()]).
header(X) -> lens:c([lens:map(fd), lens:t2(), lens:t3(), lens:pair(X, none)]).

new() ->
   m_state:put(id(), undefined).

new(Id) -> 
   m_state:put(id(), scalar:s(Id)).

%% @todo: stream as tcp
%% @todo: method for method (method triggers request)
method(Mthd) ->
   % @todo: validate and normalize method
   m_state:put(method(), Mthd).

url(Url) ->
   m_state:put(url(), uri:new(Url)).

header(Head, Value) ->
   % @todo: fix htstream to accept various headers
   m_state:put(header(scalar:atom(Head)), scalar:s(Value)).

%%
%% http actions
thinktime(T) ->
   fun(State) ->
      [timer:sleep(T)|State]
   end.

request() ->
   fun(#{fd := Http} = State0) ->
      {Sock, State1} = socket(State0),
      knet:send(Sock, lens:get(lens:t2(), Http)),
      %% @todo: remove authority if connection is not keep/alive
      [recv(Sock)|State1]
   end.

request(Pckt) ->
   fun(State0) ->
      #{fd := Http} = State1 = lens:put(header('Transfer-Encoding'), <<"chunked">>, State0),
      {Sock, State2} = socket(State1),
      knet:send(Sock, lens:get(lens:t2(), Http)),
      knet:send(Sock, Pckt),
      knet:send(Sock, eof),
      %% @todo: remove authority if connection is not keep/alive
      [recv(Sock)|State2]
   end.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%%
authority(State) ->
   uri:authority(lens:get(url(), State)).

%%
%%
socket(State) ->
   case 
      maps:get(authority(State), State, undefined)  
   of
      undefined ->
         Sock = connect(State),
         {Sock, State#{authority(State) => Sock}};
      Sock ->
         {Sock, State}
   end.

connect(State) ->
   %% @todo: configurable io timeout
   % % Sock = knet:socket(lens:get(url(), State), [{trace, self()}]),
   % Sock = knet:socket(lens:get(url(), State), []),
   % {ioctl, b, Sock} = knet:recv(Sock),
   Uri = lens:get(url(), State),
   {ok, Sock} = supervisor:start_child(njord_sup, [Uri]),
   pipe:bind(a, Sock),
   Sock.

%%
%%
recv(Sock) ->
   %% @todo: customizable timeout by client
   case knet:recv(Sock, 30000) of
      {http, Sock,  eof} ->
         [];
      {http, Sock, Pckt} ->
         [Pckt | recv(Sock)]
   end.

