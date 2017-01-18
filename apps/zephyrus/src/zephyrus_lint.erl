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
   'POST'/3
]).

%%
allowed_methods(_Req) ->
   ['POST'].

%%
content_provided(_Req) ->
   [{application, erlang}].

%%
content_accepted(_Req) ->
   [{application, erlang}].

%%
%%
'POST'(_, Scenario, {_Url, _Head, Env}) ->
   [$^||
      identity(Env),
      compile(_, Scenario),
      install(_),
      scenario:t(_),
      execute(_)
   ].

%%
identity(Env) ->
   {ok, scalar:atom( lens:get(lens:pair(<<"id">>), Env) )}.

%%
compile(Id, Scenario) ->
   File = file(Id),
   ok = filelib:ensure_dir(File),
   ok = file:write_file(File, Scenario),
   case scenario:c(Id, File) of
      {ok, Id, Code} ->
         {ok, {Id, Code}};
      {error, Error, Warn} ->
         ErrorPretty = prettify_error_or_warn(Error),
         WarnPretty = prettify_error_or_warn(Warn),
         ResultBody = jsx:encode([
            {errors, ErrorPretty},
            {warnings, WarnPretty}
         ]),
         {error, {badarg, ResultBody}}
   end.

prettify_error_or_warn(X) ->
  Result1 = lists:flatmap(
    fun({_, Second}) ->
      Second
    end, X),

  Result2 = lists:map(
    fun({LineNumber, _, Args}) ->
      Message = erlang:iolist_to_binary(erl_lint:format_error(Args)),
      [
        {lineNumber, LineNumber},
%%            {args, Args},
        {message, Message}
      ]
    end, Result1),

  Result2.


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
%% @deprecated, 'Mio' support is over at 0.10.x release
config(Scenario) ->
   case lens:get(lens:pair(init, undefined), Scenario:module_info(exports)) of
      0 ->
         State = #{pool => fun netpool/2, peer => []},
         [Conf|_] = (Scenario:init())(State),
         Conf;
      _ ->
         undefined
   end.

%%
%% @deprecated, 'Mio' support is over at 0.10.x release
lint(Scenario, Conf) ->
   State = #{pool => fun netpool/2, peer => []},
   case (Scenario:run(Conf))(State) of
      [[{_, _, _, _}|Data]|_] ->
         jsx:encode(erlang:iolist_to_binary(Data));

      [Data|_] ->
         erlang:iolist_to_binary(Data)
         % jsx:encode(Data)
   end.

%%
%%
file(Id) ->
   filename:join([opts:val(libdir, typhoon), scalar:c(Id) ++ ".erl"]).

%%
%% selector of net i/o pool (hack uses lint pool)
netpool(Url, Header) ->
   Id = scalar:atom(uri:s(uri:suburi(<<"lint">>, Url))),
   case erlang:whereis(Id) of
      undefined ->
         typhoon_net_sup:spawn(Id, disposable, Url),
         netpool(Url, Header);
      Pid ->
         Pid
   end.   

