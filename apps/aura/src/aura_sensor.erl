%% @doc
%% 
-module(aura_sensor).
-behaviour(pipe).

-export([
   start_link/2,
   init/1,
   free/2,
   handle/3
]).

%%
-record(state, {
   heap = undefined :: datum:heap()
}).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

start_link(Ns, Peer) ->
   pipe:start_link(?MODULE, [Ns, Peer], []).


init([Ns, Peer]) ->
   ok = pns:register(Ns, Peer, self()),
   {ok, handle,
      #state{
         heap = heap:new()
      }
   }.

free(_, _State) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% pipe
%%%
%%%----------------------------------------------------------------------------   

handle({put, _, {Key, Value}}, Pipe, #state{heap = Heap} = State) ->
   pipe:ack(Pipe, ok),
   {next_state, handle, State#state{heap = heap:insert(Key, Value, Heap)}};

handle({get, _}, Pipe, #state{heap = Heap} = State) ->
   pipe:ack(Pipe, heap:list(Heap)),
   {next_state, handle, State};

handle({remove, _}, Pipe, #state{heap = Heap} = State) ->
   pipe:ack(Pipe, heap:list(Heap)),
   {stop, normal, State}.
