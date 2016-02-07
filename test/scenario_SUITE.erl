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
-export([
   context/1, 
   http_get/1, http_post/1, http_eval/1
]).

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
         context,
         http_get, http_post, http_eval
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

context(Config) ->
   Spec = json(context, Config),
   Scenario = scenario:compile(Spec),
   {[],  _} = scenario:eval(Scenario).


http_get(Config) ->
   Spec = json(http_get, Config),
   Scenario = scenario:compile(Spec),

   10   = scenario:n(Scenario),  
   600  = scenario:t(Scenario),  
   {[Http, eof],   _} = scenario:eval(Scenario),

   {'GET', Url, Head} = Http,
   <<"http://localhost:8888/a">> = uri:s(Url),
   <<"typhoon/0.0.0">> = lens:get(lens:pair(<<"User-Agent">>), Head),   
   <<"*/*">> = lens:get(lens:pair(<<"Accept">>), Head),   
   <<"xvalue">> = lens:get(lens:pair(<<"X-Header">>), Head).


http_post(Config) ->
   Spec = json(http_post, Config),
   Scenario = scenario:compile(Spec),
   10   = scenario:n(Scenario),  
   600  = scenario:t(Scenario),  
   {[Http, Chunk, eof],   _} = scenario:eval(Scenario),

   {'POST', Url, Head} = Http,
   <<"http://localhost:8888/a">> = uri:s(Url),
   <<"typhoon/0.0.0">> = lens:get(lens:pair(<<"User-Agent">>), Head),   
   <<"*/*">> = lens:get(lens:pair(<<"Accept">>), Head),   
   <<"xvalue">> = lens:get(lens:pair(<<"X-Header">>), Head),

   <<"0123456789">> = Chunk.


http_eval(Config) ->
   Spec = json(http_eval, Config),
   Scenario = scenario:compile(Spec),
   {[Http, Chunk, eof],   _} = scenario:eval(Scenario),

   {'POST', Url, Head} = Http,
   [Int, Ascii, Uid] = uri:segments(Url),
   true  = is_integer( scalar:i(Int) ),
   Ascii = << <<X:8>> || <<X:8>> <= Ascii, is_ascii(X) >>,
   Uid   = << <<X:8>> || <<X:8>> <= Uid, is_hex(X) >>,

   <<"typhoon/", Vsn/binary>> = lens:get(lens:pair(<<"User-Agent">>), Head), 
   true  = is_integer( scalar:i(Int) ),
  
   Value = lens:get(lens:pair(<<"X-Header">>), Head),
   Value = << <<X:8>> || <<X:8>> <= Value, is_ascii(X) >>.
   

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
json(Id, Config) ->
   File = filename:join([?config(data_dir, Config), scalar:c(Id) ++ ".json"]),
   {ok, Json} = file:read_file(File),
   jsx:decode(Json).

%%
is_ascii(X)
 when X >= $a, X =< $z ->
   true;
is_ascii(X)
 when X >= $A, X =< $Z ->
   true;
is_ascii(X)
 when X >= $0, X =< $9 ->
   true;
is_ascii(_) ->
   false.

%%
is_hex(X)
 when X >= $a, X =< $f ->
   true;
is_hex(X)
 when X >= $0, X =< $9 ->
   true;
is_hex(_) ->
   false.

%%
is_text(X)
 when X >= $a, X =< $z ->
   true;
is_text(X)
 when X =:= $  ->
   true;
is_text(_) ->
   false.


