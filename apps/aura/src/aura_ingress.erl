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
-module(aura_ingress).
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
   {ok, handle, #{fd => aura:fd(), tth => 0}}.

free(_, _) ->
   ok.

ioctl(_, _) ->
   throw(not_implemented).

%%%----------------------------------------------------------------------------   
%%%
%%% pipe
%%%
%%%----------------------------------------------------------------------------   

handle(run, _, #{tth := TTH} = State) ->
   case kmq:deq(auraq, 10) of
      [] ->
         % erlang:send(self(), run),
         erlang:send_after(50, self(), run),
         % erlang:send_after(TTH, self(), run),
         {next_state, handle, State#{tth => erlang:max(10000, TTH + 1)}};
      List ->
         Tx = [pts:cast(aura_sensor, Urn, {T, Val}) 
                  || X <- List,
                     {{urn, _, _} = Urn, T, Val} <- [aura_protocol:decode(X)]],
         clue:inc({aura, ingress}, length(List)),
         {next_state, handle, State#{tx => Tx, tth => 0}}
   end;

handle({{urn, <<"clue">>, Key}, _T, X}, Pipe, State) ->
   clue:inc(Key, X),
   pipe:ack(Pipe, ok),
   {next_state, handle, State};

% handle({{urn, _, _} = Urn, T, X}, Pipe, #{fd := FD} = State) ->
%    spawn(
%       fun() ->
%          chronolog:append(FD, Urn, [{T, X}]),
%          pipe:ack(Pipe, ok),
%          clue:inc({aura, ingress})
%       end
%    ),
%    {next_state, handle, State};

handle({Tx, ok}, _Pipe, #{tx := List0} = State) ->
   case lists:delete(Tx, List0) of
      [] ->
         erlang:send(self(), run),
         {next_state, handle, maps:remove(tx, State)};
      List1 ->
         {next_state, handle, State#{tx => List1}}
   end.
         

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   
   
