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
-module(aura_io).
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
   pipe:start_link(?MODULE, [], []).

init([]) ->
   erlang:send(self(), run),
   {ok, handle, #{fd => typhoon:fd()}}.

free(_, _) ->
   ok.

ioctl(_, _) ->
   throw(not_implemented).

%%%----------------------------------------------------------------------------   
%%%
%%% pipe
%%%
%%%----------------------------------------------------------------------------   

handle(run, _, State) ->
   case kmq:deq(auraq, 10) of
      [] ->
         erlang:send_after(5000, self(), run),
         {next_state, handle, State};
      List ->
         [erlang:send(self(), aura:decode(X)) || X <- List],
         erlang:send(self(), run),
         {next_state, handle, State}
   end;
   
handle({{urn, _, _} = Urn, T, X}, _, #{fd := FD} = State) ->
   spawn(
      fun() ->
         chronolog:append(FD, Urn, [{T, X}])
      end
   ),
   {next_state, handle, State}.
