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
-module(zephyrus_lint).
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
   [{'application', 'json'}].

%%
content_accepted(_Req) ->
   [{application, erlang}, {application, json}].

%%
fail({badarg, Reason}, _Req) ->
   {badarg, Reason}.   

%%
%%
'POST'({_, Type}, Scenario, {_Url, _Head, Env}) ->
   [$^||
      identity(Env),
      compile(_, Type, Scenario),
      install(_),
      scenario:t(_),
      execute(_)
   ].

%%
identity(Env) ->
   {ok, scalar:atom( lens:get(lens:pair(<<"id">>), Env) )}.

%%
compile(Id, {application, json}, Scenario) ->
   Json = [{scalar:atom(Key), Val} || {Key, Val} <- jsx:decode(iolist_to_binary(Scenario))],
   {ok, File} = file:read_file(filename:join([code:priv_dir(zephyrus), "typhoon.swirl"])),
   Fun0 = swirl:f(File),
   Fun1 = Fun0(undefined),
   compile(Id, {application, erlang}, Fun1([{id, Id} | Json]));


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
execute(Id) ->
   try
      {ok, lint(Id, config(Id))}
   catch Error:Reason ->
      {error, {badarg, scalar:s(io_lib:format("~nErrors:~n~p:~p~n~p~n", [Error, Reason, erlang:get_stacktrace()]))}}
   end.

%%
%% 
config(Scenario) ->
   [$? ||
      scenario:option(init, Scenario),
      fmap(_(#{})),
      fmap(hd(_))
   ].

%%
%%
lint(Scenario, Conf) ->
   case (Scenario:run(Conf))(#{}) of
      [[{Code, Text, _, _}|Data]|_] ->
         jsx:encode([
            {code, Code},
            {text, Text},
            {data, erlang:iolist_to_binary(Data)}
         ]);
      [Data|_] ->
         jsx:encode([
            {data, erlang:iolist_to_binary(Data)}
         ])
   end.

%%
%%
file(Id) ->
   filename:join([opts:val(libdir, typhoon), scalar:c(Id) ++ ".erl"]).
