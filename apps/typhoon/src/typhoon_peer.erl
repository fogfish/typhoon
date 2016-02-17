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
   {ok, handle, #{fd => aura:fd()}}.

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


handle(_, _, State) ->
   {next_state, handle, State}.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

