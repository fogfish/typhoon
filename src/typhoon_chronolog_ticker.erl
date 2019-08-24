-module(typhoon_chronolog_ticker).
-behaviour(pipe).
-compile({parse_transform, category}).

-export([
   start_link/2,
   init/1,
   free/2,
   handle/3
]).

-record(state, {
   heap = undefined :: datum:heap()
}).

%%-----------------------------------------------------------------------------
%%
%% factory
%%
%%-----------------------------------------------------------------------------

start_link(Ns, Uid) ->
   pipe:start_link({via, pns, {urn, Ns, Uid}}, ?MODULE, [], []).

init(_) ->
   {ok, handle, 
      #state{
         heap = heap:new()
      }
   }.

free(_, _) ->
   ok.

%%-----------------------------------------------------------------------------
%%
%% handle
%%
%%-----------------------------------------------------------------------------

handle({put, _, {T, Value}}, _, #state{heap = Heap} = State) ->
   {reply, ok, 
      State#state{
         heap = heap:insert(T, Value, Heap)
      }
   };

handle({get, _}, Pipe, #state{heap = Heap} = State) ->
   pipe:ack(Pipe, heap:list(Heap)),
   {next_state, handle, State};

handle({remove, _}, Pipe, #state{heap = Heap} = State) ->
   pipe:ack(Pipe, heap:list(Heap)),
   {stop, normal, State}.

