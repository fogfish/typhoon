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
%%   
-module(aura_storage).
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
   [Node, _Host] = string:tokens(scalar:c(erlang:node()), "@"),
   File = filename:join([opts:val(vardir, aura), Node]),
   {ok, FD} = chronolog:new([
      persistent,

      %% eleveldb options 
      {file, File},
      {write_buffer_size, 16 * 1024 * 1024},
      {total_leveldb_mem_percent,       40},
      {eleveldb_threads,                32},
      {sync, false},

      %% read/write thought cache
      {cache, [
         {n,      4},
         {ttl,    3600},
         {memory, 100 * 1024 * 1024}
      ]}
   ]),
   {ok, _} = ek:create(aura, opts:val(ring, aura)),
    ok     = ek:join(aura, scalar:s(erlang:node()), self()),
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

handle({stream, Gen}, Pipe, #{fd := FD} = State) ->
   spawn(
      fun() ->
         List = stream:list( Gen(FD) ),
         pipe:ack(Pipe, {ok, List})
      end
   ),
   {next_state, handle, State};

handle({lookup, Urn}, Pipe, #{fd := FD} = State) ->
   pipe:ack(Pipe, chronolog:lookup(FD, Urn)),
   {next_state, handle, State};

handle(_, _, State) ->
   {next_state, handle, State}.


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

