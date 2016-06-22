-module(aura_sensor).
-behaviour(pipe).

-export([
   start_link/2,
   init/1,
   free/2,
   dirty/3,
   fresh/3
]).

%% see 
%%   https://en.wikipedia.org/wiki/Exponential_smoothing
-define(A, 0.01).


%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

start_link(Ns, Urn) ->
   pipe:start_link(?MODULE, [Ns, Urn], []).


init([Ns, {urn, _, _} = Urn]) ->
   ok = pns:register(Ns, Urn, self()),
   {ok, dirty,
      #{
         fd   => aura:fd(),
         urn  => Urn,
         x    => 0.0,
         t    => os:timestamp(),
         tts  => tempus:timer(1000, tts)   
      }
   }.

free(_, _State) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% pipe
%%%
%%%----------------------------------------------------------------------------   

%%
%%
dirty({_, _} = X, Pipe, State) ->
   pipe:ack(Pipe, ok),
   {next_state, fresh, update(X, State)};

dirty(tts, _Pipe, #{tts := TTS} = State) ->
   {next_state, dirty, 
      State#{
         tts => tempus:reset(TTS, tts)
      }
   }.

%%
%%
fresh({_, _} = X, Pipe, State) ->
   pipe:ack(Pipe, ok),
   {next_state, fresh, update(X, State)};

fresh(tts, _Pipe, #{tts := TTS} = State0) ->
   State = append(State0),
   {next_state, dirty, 
      State#{
         tts => tempus:reset(TTS, tts)
      }
   }.      

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%%
update({_, X}, #{urn := {urn, <<"g">>, _}, x := X0} = State) ->
   State#{
      t => os:timestamp(), 
      x => ?A * X + (1 - ?A) * X0
   };

update({_, X}, #{urn := {urn, <<"c">>, _}, x := X0} = State) ->
   State#{
      t => os:timestamp(), 
      x => X0 + X
   }.

%%
%%
append(#{urn := {urn, <<"g">>, _} = Urn, t := T, x := X, fd := FD} = State) ->
   chronolog:append(FD, Urn, [{T, erlang:trunc(X)}]),
   State;

append(#{urn := {urn, <<"c">>, _} = Urn, t := T, x := X, fd := FD} = State) ->
   chronolog:append(FD, Urn, [{T, erlang:trunc(X)}]),
   State#{x => 0.0}.

