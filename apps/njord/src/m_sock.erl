%% @doc
%%   socket IO monad
-module(m_sock).

-export([return/1, fail/1, '>>='/2]).
-export([new/0, new/1, url/1, thinktime/1]).
-export([send/1, recv/0, request/1]).


%%%----------------------------------------------------------------------------   
%%%
%%% state monad
%%%
%%%----------------------------------------------------------------------------   

return(X) -> 
   m_state:return(X).

fail(X) ->
   m_state:fail(X).

'>>='(X, Fun) ->
   m_state:'>>='(X, Fun).


%%%----------------------------------------------------------------------------   
%%%
%%% socket monad
%%%
%%%----------------------------------------------------------------------------   

%%
%% socket specification
id()   -> lens:c([lens:map(fd,  {none, none}), lens:t1()]).
url()  -> lens:c([lens:map(fd), lens:t2()]).

new() ->
   m_state:put(id(), undefined).

new(Id) -> 
   m_state:put(id(), scalar:s(Id)).

url(Url) ->
   m_state:put(url(), uri:new(Url)).

%%
%% socket actions
thinktime(T) ->
   fun(State) ->
      [timer:sleep(T)|State]
   end.

%%
%% send packet
send(Pckt) ->
   fun(State) ->
      Sock = socket(State),
      [knet:send(Sock, Pckt)|State#{authority(State) => Sock}]
   end.

%%
%% recv packet
recv() ->
   fun(State) ->
      Sock = socket(State),
      [recv(Sock)|State#{authority(State) => Sock}]
   end.

%%
%% send / recv packet
request(Pckt) ->
   fun(State) ->
      Sock = socket(State),
      knet:send(Sock, Pckt),
      %% @todo: remove authority if connection is not keep/alive
      [recv(Sock)|maps:remove(fd, State#{authority(State) => Sock})]
   end.

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
         connect(State);
      Sock ->
         Sock
   end.

connect(State) ->
   %% @todo: configurable io timeout
   Sock = knet:connect(lens:get(url(), State)),
   {ioctl, b, Sock} = knet:recv(Sock),
   {_, Sock, {established, _}} = knet:recv(Sock, 30000),
   Sock.

%%
%%
recv(Sock) ->
   %% @todo: customizable timeout by client
   {_, Sock, Pckt} = knet:recv(Sock, 30000),
   Pckt.
