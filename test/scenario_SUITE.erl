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
-module(scenario_SUITE).
-include_lib("common_test/include/ct.hrl").

%% common test
-export([
   all/0 ,groups/0
  ,init_per_suite/1 ,end_per_suite/1
  ,init_per_group/2 ,end_per_group/2
]).
-export([compile/1, validate/1, lint/1, make/1]).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

all() ->
   [
      {group, scenario}
   ].

groups() ->
   [
      {scenario, [parallel], [
         compile, validate, lint, make
      ]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   

%%
init_per_suite(Config) ->
   ok = application:start(uid),
   ok = application:start(scenario),
   ok = knet:start(),
   Config.

end_per_suite(_Config) ->
   ok.

%%
init_per_group(_, Config) ->
   Config.

end_per_group(_, _Config) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% unit test
%%%
%%%----------------------------------------------------------------------------   

%%
%%
compile(Config) ->
   Scenario = filename:join([?config(data_dir, Config), "unittest.erl"]),
   {ok, unittest_compile, _} = scenario:c(unittest_compile, Scenario).

%%
%%
validate(Config) ->
   Scenario = filename:join([?config(data_dir, Config), "unittest.erl"]),
   {ok, unittest_validate, Code} = scenario:c(unittest_validate, Scenario),
   load_scenario_code(unittest_validate, Code),
   {ok, _} = scenario:t(unittest_validate).

%%
%%
lint(Config) ->
   Scenario = filename:join([?config(data_dir, Config), "unittest.erl"]),
   {ok, unittest_lint, Code} = scenario:c(unittest_lint, Scenario),
   load_scenario_code(unittest_lint, Code),
   {200, _, Spec} = scenario:lint(unittest_lint).

%%
%%
make(Config) ->
   Scenario   = filename:join([?config(data_dir, Config), "unittest.erl"]),
   {ok, Spec} = file:read_file(Scenario),
   {ok, _} = scenario:make(unittest_make, Spec).

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

load_scenario_code(Id, Code) ->
   code:purge(Id),
   {module, Id} = code:load_binary(Id, undefined, Code).

