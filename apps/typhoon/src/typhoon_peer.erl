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
%%   application peer
-module(typhoon_peer).
-behaviour(pipe).
-author('dmitry.kolesnikov@zalando.fi').

-export([
   start_link/0
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

start_link() ->
   pipe:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
   {ok, _} = clot:seed(),
   [Node, _Host] = string:tokens(scalar:c(erlang:node()), "@"),
   File = filename:join([opts:val(vardir, typhoon), Node]),
   {ok, FD} = chronolog:new([persistent, {file, File}]),
   {ok, handle, #{fd => FD}}.

free(_, _) ->
   ok.

ioctl(fd, #{fd := FD}) ->
   FD.

%%%----------------------------------------------------------------------------   
%%%
%%% pipe
%%%
%%%----------------------------------------------------------------------------   

handle({series, Urn, Filter, Chronon, Range}, Pipe, #{fd := FD} = State) ->
   spawn(
      fun() ->
         pipe:ack(Pipe, series(FD, Urn, Filter, Chronon, Range))
      end
   ),
   {next_state, handle, State};

handle({metric, Urn, Chronon, Range}, Pipe, #{fd := FD} = State) ->
   spawn(
      fun() ->
         pipe:ack(Pipe, metric(FD, Urn, Chronon, Range))
      end
   ),
   {next_state, handle, State};

handle({status, Urn, Code, Chronon, Range}, Pipe, #{fd := FD} = State) ->
   spawn(
      fun() ->
         pipe:ack(Pipe, status(FD, Urn, Code, Chronon, Range))
      end
   ),
   {next_state, handle, State};

handle(_, _, State) ->
   {next_state, handle, State}.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% build a series stream
series(FD, Urn, mean, Chronon, Range) ->
   stream:list(
      chronolog:scan(
         fun(X) -> lists:sum(X) div length(X) end,
         Chronon,
         chronolog:stream(FD, Urn, Range)
      )
   );

series(FD, Urn, max, Chronon, Range) ->
   stream:list(
      chronolog:scan(
         fun(X) -> lists:max(X) end,
         Chronon,
         chronolog:stream(FD, Urn, Range)
      )
   );

series(FD, Urn, {pth, Pth}, Chronon, Range) ->
   stream:list(
      chronolog:scan(
         fun(X) -> lists:nth(round(length(X) * Pth), lists:sort(X)) end,
         Chronon,
         chronolog:stream(FD, Urn, Range)
      )
   ).

%%
%% samples per second
metric(FD, Urn, Chronon, Range) ->
   stream:list(
      chronolog:scan(
         fun(X) -> length(X) / Chronon end,
         Chronon,
         chronolog:stream(FD, Urn, Range)
      )
   ).

%%
%% samples per second (with given filter)
status(FD, Urn, Code, Chronon, Range) ->
   stream:list(
      chronolog:scan(
         fun(X) -> length(X) / Chronon end,
         Chronon,
         stream:filter(
            fun({_, X}) -> X >= Code andalso X < ((Code div 100) + 1) * 100 end,
            chronolog:stream(FD, Urn, Range)
         )
      )
   ).


