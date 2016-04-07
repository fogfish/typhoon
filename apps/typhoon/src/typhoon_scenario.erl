%%
%%   Copyright 2015 Zalando SE
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
%%   load scenario controller
-module(typhoon_scenario).
-behaviour(pipe).
-author('dmitry.kolesnikov@zalando.fi').

-export([
   start_link/3
  ,init/1
  ,free/2
  ,ioctl/2
  ,handle/3
]).
%% ambit callback
-export([
   process/1,
   handoff/2
]).

%%%----------------------------------------------------------------------------   
%%%
%%% Factory
%%%
%%%----------------------------------------------------------------------------   

start_link(Vnode, Name, Spec) ->
   pipe:start_link(?MODULE, [Vnode, Name, Spec], []).

init([Vnode, Name, Spec]) ->
   clue:define(meter, Name, 60000),
   {ok, handle, 
      #{
         vnode => Vnode,
         name  => Name,
         spec  => Spec,
         n     => 0
      }
   }.

free(_, _) ->
   ok.

ioctl(n, #{n := N}) ->
   N.

%%%----------------------------------------------------------------------------   
%%%
%%% ambit
%%%
%%%----------------------------------------------------------------------------   

process(Root) ->
   {ok, Root}.

handoff(Root, Vnode) ->
   pipe:call(Root, {handoff, Vnode}).

%%%----------------------------------------------------------------------------   
%%%
%%% pipe
%%%
%%%----------------------------------------------------------------------------   

handle({handoff, _Vnode}, Tx, State) ->
   pipe:ack(Tx, ok),
   {next_state, handle, State};

handle(run, Tx, #{n := N0}=State) ->
   N1 = run(
      q:new([erlang:node() | erlang:nodes()]), 
      State
   ),
   pipe:ack(Tx, {ok, N1}),
   {next_state, handle, State#{n => N0 + N1}};

handle({'DOWN', _Ref, _Type, _Pid, _Reason}, _, #{n := N} = State) ->
   {next_state, handle, State#{n := N - 1}};

handle(_Msg, _Tx, State) ->
   {next_state, handle, State}.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% run test case on cluster
run(Nodes, #{spec := Spec} = State) ->
   run(pair:x(<<"n">>, Spec), 0, Nodes, State).

run(0, K, _Nodes, _State) ->
   K;
run(N, K, Nodes, #{name :=Name, spec := Spec}=State) ->
   case 
      supervisor:start_child({typhoon_unit_sup, q:head(Nodes)}, [Name, Spec]) 
   of
      {ok, Pid} ->
         erlang:monitor(process, Pid),
         run(N - 1, K + 1, q:enq(q:head(Nodes), q:tail(Nodes)), State);
      {error,_} ->
         run(N - 1, K, q:enq(q:head(Nodes), q:tail(Nodes)), State)
   end.

%%
%% define counters



