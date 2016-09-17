-module(m_tcp).

-export([return/1, fail/1, '>>='/2]).
-export([new/0, new/1, url/1]).

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
%%% tcp monad
%%%
%%%----------------------------------------------------------------------------   

id()      -> lens:c([lens:map(tcp,  {none, none}), lens:t1()]).
url()     -> lens:c([lens:map(tcp), lens:t2()]).

new() ->
   m_state:put(id(), undefined).

new("urn:tcp:" ++ _ = Id) -> 
   m_state:put(id(), scalar:s(Id)).

url(Url) ->
   m_state:put(url(), uri:new(Url)).


send(Pckt) ->
   fun(State) ->
      Sock = socket(State),
      [knet:send(Sock, Pckt)|State#{authority(State) => Sock}]
   end.

recv() ->
   fun(State) ->
      Sock = socket(State),
      [recv(Sock)|maps:remove(tcp, State#{authority(State) => Sock})]
   end.

request(Pckt) ->
   fun(State) ->
      Sock = socket(State),
      knet:send(Sock, Pckt),
      %% @todo: remove authority if connection is not keep/alive
      [recv(Sock)|maps:remove(tcp, State#{authority(State) => Sock})]
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
         socket_new(State);
      Sock ->
         Sock
   end.

socket_new(State) ->
   %% @todo: configurable io timeout
   Sock = knet:connect(lens:get(url(), State)),
   {ioctl, b, Sock} = knet:recv(Sock),
   {tcp, Sock, {established, _}} = knet:recv(Sock, 30000),
   Sock.

%%
%%
recv(Sock) ->
   %% @todo: customizable timeout by client
   {tcp, Sock, Pckt} = knet:recv(Sock, 30000),
   Pckt.

   % case knet:recv(Sock, 30000) of
   %    {tcp, Sock,  eof} ->
   %       [];
   %    {tcp, Sock, Pckt} ->
   %       [Pckt | recv(Sock)]
   % end.
