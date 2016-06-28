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
-module(aura_sensor).
-behaviour(pipe).

-export([
   start_link/2,
   init/1,
   free/2,
   handle/3
]).

%% see 
%%   https://en.wikipedia.org/wiki/Exponential_smoothing
-define(A, 0.2).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

start_link(Ns, Urn) ->
   pipe:start_link(?MODULE, [Ns, Urn], []).


init([Ns, {urn, _, _} = Urn]) ->
   ok = pns:register(Ns, Urn, self()),
   {ok, handle,
      #{
         fd   => aura:fd(),
         urn  => Urn,
         t    => {0, 0, 0},
         x    => 0.0
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
handle({get, _}, Pipe, #{x := X} = State) ->
   pipe:ack(Pipe, {ok, X}),
   {next_state, handle, State};

handle({{A, B, _}, X}, Pipe, #{t := {A, B, _}} = State) ->
   pipe:ack(Pipe, ok),
   {next_state, handle, update(X, State)};

handle({{A0, A1, _} = A, _} = X, Pipe, #{t := B} = State)
 when A > B ->
   State0 = append(State),
   handle(X, Pipe, State0#{t := {A0, A1, 0}});

handle({_, _}, Pipe, State) ->
   pipe:ack(Pipe, ok),
   {next_state, handle, State}.


%%
append(#{t := {0, 0, 0}} = State) ->
   State;
append(#{fd := FD, urn := Urn, t := T, x := X} = State) ->
   chronolog:append(FD, Urn, [{T, erlang:trunc(X)}]),
   State#{t => {0, 0, 0}, x => 0.0}.

%%
update(X, #{urn := {urn, <<"g">>, _}, x := X0} = State) ->
   State#{x => ?A * X + (1 - ?A) * X0};

update(X, #{urn := {urn, <<"c">>, _}, x := X0} = State) ->
   State#{x => X0 + X};

update(_, State) ->
   State.

