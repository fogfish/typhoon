%% @doc
%%   socket IO monad
-module(m_sock).

-export([return/1, fail/1, '>>='/2]).
-export([new/0, new/1, url/1, thinktime/1]).
-export([send/1, recv/0, recv/1]).


%%%----------------------------------------------------------------------------   
%%%
%%% state monad
%%%
%%%----------------------------------------------------------------------------   

return(X) -> 
   % m_state:return(X).
   fun(State) -> [X|maps:remove(fd, State)] end.

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
   fun(State0) ->
      {Sock, State1} = socket(State0),
      [knet:send(Sock, Pckt)|State1]
   end.

%%
%% recv packet
recv() ->
   recv(30000).

recv(Timeout) ->
   fun(State0) ->
      {Sock, State1} = socket(State0),
      [recv(Sock, Timeout)|State1]
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
   %% @todo: configurable i/o timeout
   Uri = lens:get(url(), State),
   {ok, Sock} = supervisor:start_child(njord_sup, [Uri]),
   pipe:bind(a, Sock),
   pipe:send(Sock, {connect, Uri}),
   {_, Sock, {established, _}} = pipe:recv(Sock, 30000, []),
   Sock.

%%
%%
recv(Sock, Timeout) ->
   {_, Sock, Pckt} = knet:recv(Sock, Timeout),
   Pckt.
