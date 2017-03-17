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
-include_lib("ambitz/include/ambitz.hrl").

-export([
   start_link/3
  ,init/1
  ,free/2
  ,ioctl/2
  ,handle/3
]).

%%%----------------------------------------------------------------------------   
%%%
%%% Factory
%%%
%%%----------------------------------------------------------------------------   

start_link(Vnode, Mod, Spec) ->
   pipe:start_link(?MODULE, [Vnode, Mod, Spec], []).

init([Vnode, Mod, Spec]) ->
   {ok, Code} = compile(Mod, Spec),
   {ok, handle, 
      #{
         vnode => Vnode,
         mod   => Mod,
         code  => Code,
         n     => 0
      }
   }.

free(_, #{mod := Mod}) ->
   file:delete(file(Mod)).

ioctl(attr, #{mod := Scenario, n := Session}) ->
   [
      {id,     Scenario},
      {t,      Scenario:t()},
      {n,      Scenario:n()},
      {title,  scalar:s(Scenario:title())},
      {session,  Session},
      {urn, [scalar:s(X) || X <- Scenario:urn()]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% pipe
%%%
%%%----------------------------------------------------------------------------   

handle(run, Tx, #{mod := Mod, code := Code, n := N0}=State) ->
   drift(Mod, Code),
   history(State),
   N1 = length(run(State)),
   pipe:ack(Tx, {ok, N1}),
   {next_state, handle, State#{n => N0 + N1}};

handle(once, Tx, #{mod := Scenario} = State) ->
   Config   = config(Scenario),
   UnitTest = [X || {X, 1} <- Scenario:module_info(exports), X =/= init, X =/= module_info],
   Value    = lists:map(
      fun(Unit) ->
         hd( (Scenario:Unit(Config))(#{}) )
      end,
      UnitTest
   ),
   pipe:ack(Tx, Value),
   {next_state, handle, State};

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
%%



%%
%% deploy code to cluster nodes
drift(Id, Code) ->
   lists:foreach(fun(Node) -> drift(Node, Id, Code) end, erlang:nodes()).

drift(Node, Mod, Code) ->
   _ = rpc:call(Node, code, purge, [Mod]),
   {module, Mod} = rpc:call(Node, code, load_binary, [Mod, undefined, Code]).

%%
%% run test case on cluster
run(#{mod := Scenario} = State) ->
   T = tempus:add(os:timestamp(), Scenario:t() div 1000),
   N = opts:val(n, opts:val(ring, ambit)),
   run(Scenario:n(), N, T, State).

run(Q, _N, _T, _State)
 when Q =< 0 ->
   [];
run(Q, N, T, #{mod := Scenario} = State) ->
   Id = uid:encode(uid:g()),
   {ok, #entity{vnode = Vnodes}} = ambitz:spawn(typhoon, Id, {typhoon_unit_sup, start_link, [Scenario, T]}, [{w, N}]),
   [Id | run(Q - length(Vnodes), N, T, State)].

%%
%% log history
history(#{mod := Scenario}) ->
   Urn  = {urn, <<"c">>, <<"scenario:", (scalar:s(Scenario))/binary>>},
   aura:send(Urn, os:timestamp(), Scenario:t()).

%%
%% configure scenario execution
config(Scenario) ->
   case lens:get(lens:pair(init, undefined), Scenario:module_info(exports)) of
      0 ->
         Fun = Scenario:init(),
         Fun(#{});

      _ ->
         [undefined]
   end.

