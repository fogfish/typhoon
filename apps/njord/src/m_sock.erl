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
id()   -> lens:c([lens:map(fd, #{}), lens:map(id,  none)]).
url()  -> lens:c([lens:map(fd, #{}), lens:map(url, none)]).

new() ->
   m_state:put(id(), undefined).

new(Id) -> 
   m_state:put(id(), uri:new(scalar:s(Id))).

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
      Url  = lens:get(url(), State0),
      {Sock, State1} = socket(Url, State0),
      knet:send(Sock, {trace, lens:get(id(), State1)}),
      [knet:send(Sock, Pckt)|State1]
   end.

%%
%% recv packet
recv() ->
   recv(30000).

recv(Timeout) ->
   fun(State0) ->
      Url  = lens:get(url(), State0),
      {Sock, State1} = socket(Url, State0),
      knet:send(Sock, {trace, lens:get(id(), State1)}),
      case recv(Sock, Timeout) of
         {ok, Pckt} -> [Pckt|State1];
         {error, _} -> [<<>>|sclose(Url, State1)]
      end
   end.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% 
socket(Uri, State) ->
   socket(uri:authority(Uri), Uri, State).

socket(Authority, Uri, State) ->
   case State of
      %% re-use existed connection 
      #{Authority := Sock} ->
         {Sock, State};

      %% create new connection
      _ ->
         Sock = socket(Uri),
         knet:send(Sock, {trace,   lens:get(id(), State)}),
         knet:send(Sock, {connect, Uri}),
         {_, Sock, {established, _}} = pipe:recv(Sock, 30000, []),
         {Sock, State#{Authority => Sock}}
   end.

socket(Uri) ->
   {ok, Sock} = supervisor:start_child(njord_sup, [Uri]),
   pipe:bind(a, Sock),
   Sock.

%%
%%
sclose(Uri, State) -> 
   sclose(uri:authority(Uri), Uri, State).

sclose(Authority, _Uri, State) ->
   case State of
      #{Authority := Sock} ->
         pipe:free(Sock),
         maps:remove(Authority, State);
      _ ->
         State
   end.


%%
%%
recv(Sock, Timeout) ->
   case knet:recv(Sock, Timeout, [noexit]) of
      {_, Sock, Pckt} -> 
         {ok, Pckt};
      %% transport error
      {error, _} = Error ->
         Error
   end.
