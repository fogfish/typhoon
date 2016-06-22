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
%%   scenario coordinator process
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

start_link(Vnode, Id, Spec) ->
   pipe:start_link(?MODULE, [Vnode, Id, Spec], []).

init([Vnode, Id, Spec]) ->
   clue:define(meter, Id, 60000),
   {ok, Code} = compile(Id, Spec),
   {ok, handle, 
      #{
         vnode => Vnode,
         id    => Id,
         code  => Code,
         n     => 0
      }
   }.

free(_, #{id := Id}) ->
   file:delete(file(Id)).

ioctl(n, #{n := N}) ->
   N;

ioctl(attr, #{id := Scenario}) ->
   [
      {t,   Scenario:t()},
      {n,   Scenario:n()},
      {urn, [scalar:s(X) || X <- Scenario:urn()]}
   ].

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

handle(run, Tx, #{id := Id, code := Code, n := N0}=State) ->
   % deploy compiled scenario code to each node
   Nodes = erlang:nodes(),
   lists:foreach(fun(Node) -> drift(Node, Id, Code) end, Nodes),
   milestone(State),
   % run scenario
   N1 = run(q:new([erlang:node() | Nodes]), State),
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
%%
file(Id) ->
   filename:join([opts:val(libdir, typhoon), scalar:c(Id) ++ ".erl"]).

%%
%% compiles specification into binary code
compile(Id, Scenario) ->
   File = file(Id),
   ok = filelib:ensure_dir(File),
   ok = file:write_file(File, Scenario),
   {ok, Id, Code} = scenario:c(Id, File),
   _  = code:purge(Id),
   {module, Id} = code:load_binary(Id, undefined, Code),
   {ok, Code}.


%%
%% deploy code to cluster nodes
drift(Node, Mod, Code) ->
   _ = rpc:call(Node, code, purge, [Mod]),
   {module, Mod} = rpc:call(Node, code, load_binary, [Mod, undefined, Code]).



%%
%% run test case on cluster
run(Nodes, #{id := Scenario} = State) ->
   run(Scenario:n(), 0, Nodes, State).

run(0, K, _Nodes, _State) ->
   K;
run(N, K, Nodes, #{id := Scenario}=State) ->
   case 
      supervisor:start_child({typhoon_unit_sup, q:head(Nodes)}, [Scenario]) 
   of
      {ok, Pid} ->
         erlang:monitor(process, Pid),
         run(N - 1, K + 1, q:enq(q:head(Nodes), q:tail(Nodes)), State);
      {error,_} ->
         run(N - 1, K, q:enq(q:head(Nodes), q:tail(Nodes)), State)
   end.

%%
%% log milestone
milestone(#{id := Scenario}) ->
   Urn  = {urn, <<"c">>, <<"scenario:", (scalar:s(Scenario))/binary>>},
   aura:send(Urn, os:timestamp(), Scenario:t()).


