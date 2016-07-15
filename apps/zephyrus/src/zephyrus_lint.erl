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
'POST'(_, {_Url, _Head, Env}, Scenario) ->
   Id = scalar:atom( lens:get(lens:pair(<<"id">>), Env) ),
   case compile(Id, Scenario) of
      {ok, Id,  Code} ->
         code:purge(Id),
         {module, Id} = code:load_binary(Id, undefined, Code),
         try
            Conf = case lens:get(lens:pair(init, undefined), Id:module_info(exports)) of
               0 ->
                  Cfun = Id:init(),
                  Cfun(#{pool => fun netpool/2, peer => []});
               _ ->
                  undefined
            end,
            Efun = Id:run(Conf),
            Data = Efun(#{pool => fun netpool/2, peer => []}),
            {ok, scalar:s(Data)}
         catch Error:Reason ->
            {badarg, scalar:s(io_lib:format("~nErrors:~n~p:~p~n~p~n", [Error, Reason, erlang:get_stacktrace()]))}
         end;

      {error, Error, Warn} ->
         {badarg, scalar:s(io_lib:format("~nErrors:~n~p~n~nWarnings:~n~p~n", [Error, Warn]))}
   end.


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
   scenario:c(Id, File).

%%
%% selector of net i/o pool (hack uses lint pool)
netpool(Url, Header) ->
   case erlang:whereis(lint) of
      undefined ->
         typhoon_net_sup:spawn(lint, disposable, Url),
         netpool(Url, Header);
      Pid ->
         Pid
   end.   

