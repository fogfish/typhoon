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
%%   rest api - validate scenario
-module(zephyrus_test).
-compile({parse_transform, category}).

-author('dmitry.kolesnikov@zalando.fi').

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   fail/2,
   'POST'/3
]).

%%
allowed_methods(_Req) ->
   ['POST'].

%%
content_provided(_Req) ->
   [{text, html}, {'*', '*'}].

%%
content_accepted(_Req) ->
   [{application, erlang}].

%%
fail({badarg, Reason}, _Req) ->
   {badarg, Reason};
fail({unavailable, Html}, _Req) ->
   {unavailable, [{'Content-Type', {text, html}}, {'Content-Length', size(Html)}], Html}.

%%
%%
'POST'({_, Type}, Scenario, {_Url, _Head, Env}) ->
   [either ||
      identity(Env),
      compile(_, Type, Scenario),
      install(_),
      execute(_),
      htmlify(_)
   ].

%%
identity(Env) ->
   {ok, scalar:atom( lens:get(lens:pair(<<"id">>), Env) )}.

%%
compile(Id, {application, erlang}, Scenario) ->
   File = file(Id),
   ok = filelib:ensure_dir(File),
   ok = file:write_file(File, Scenario),
   case scenario:c(Id, File) of
      {ok, Id, Code} ->
         {ok, {Id, Code}};
      {error, Error, Warn} ->
         {error, {badarg, jsonify(error, Error) ++ jsonify(warning, Warn)}}
   end.

%%
%%
jsonify(Type, Message) ->
   [$.||
      lists:flatmap(fun({_, X}) -> X end, Message),
      lists:map(fun jsonify_compiler_message/1, _),
      lists:map(fun(X) -> X#{type => Type} end, _)
   ].

jsonify_compiler_message({none, compile, _}) ->
   #{line => 1, message => "Unknown compiler error."};

jsonify_compiler_message({Line, _, Text}) when is_list(Text) ->
   #{line => Line, message => erlang:iolist_to_binary(Text)};

jsonify_compiler_message({Line, _, Text}) ->
   #{line => Line, message => erlang:iolist_to_binary(erl_lint:format_error(Text))}.

%%
install({Id, Code}) ->
   code:purge(Id),
   {module, Id} = code:load_binary(Id, undefined, Code),
   {ok, Id}.   

%%
execute(Scenario) ->
   {ok, [$. ||
      config(Scenario),
      smoketests(Scenario:module_info(exports), Scenario, _)
   ]}.

smoketests(Exports, Scenario, Config) ->
   Tests = [Id || {Id, Arity} <- Exports, Id =/= init, Id =/= module_info, Arity =:= 1],
   lists:map(
      fun(Test) ->
         smoketest(Test, Scenario, Config)
      end,
      Tests
   ).

smoketest(Test, Scenario, Config) ->
   Json = hd( (Scenario:Test(Config))(#{}) ),
   Json#{title => scalar:s(Test)}.


%%
%% 
config(Scenario) ->
   [option ||
      scenario:option(init, Scenario),
      fmap(_(#{})),
      fmap(hd(_))
   ].


%%
%%
htmlify(Json) ->
   File = filename:join([code:priv_dir(zephyrus), "smoketest.html"]),
   {ok, Html} = file:read_file(File),
   Fun0 = swirl:f(Html),
   Fun1 = Fun0(undefined),
   case json(Json) of
      #{fail := 0} = Smoked ->
         {ok, Fun1(Smoked)};

      Smoked ->
         {error, {unavailable, Fun1(Smoked)}}
   end.

json(Json) ->
   Check = lists:flatmap(fun(#{checks := X}) -> X end, Json),
   {Pass, Fail} = lists:partition(fun(#{pass := X}) -> X end, Check),
   #{size => length(Check), pass => length(Pass), fail => length(Fail), tests => Json}.


%%
%%
file(Id) ->
   filename:join([opts:val(libdir, typhoon), scalar:c(Id) ++ ".erl"]).
