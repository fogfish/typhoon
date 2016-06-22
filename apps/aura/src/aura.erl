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
-module(aura).

-export([start/0]).
-export([
   % socket/0
   ensure/1
  ,send/3
  % ,clue/3
  % ,clue/3
  ,fd/0
]).

%% udp port base
-define(PORT,  20100).

%%
%% RnD node start
start() ->
   applib:boot(?MODULE, 
      code:where_is_file("dev.config")
   ).

%%
%% send telemetry for processing
-spec send(uri:urn(), tempus:t(), number()) -> ok.

send(Urn, T, X) ->
   pts:send(aura_stream, Urn, {T, X}).

% send(Sock, Peers, {_, _, _} = Telemetry) ->
%    pipe:send(Sock, {Peers, Telemetry}).

ensure(Urn) ->
   pts:ensure(aura_stream, Urn).   


%%
%% allocate egress socket
-spec socket() -> pid().

socket() ->
   Socks = [erlang:element(2, X) || X <- supervisor:which_children(aura_egress_sup)],
   lists:nth( random:uniform(length(Socks)), Socks ).
   

% %%
% %% send KPI-counter to peers
% -spec clue(pid(), [_], _) -> ok.

% clue(Sock, Peers, Key) ->
%    clue(Sock, Peers, Key, 1).
   
% clue(Sock, Peers, Key, Val) ->
%    Urn = {urn, <<"clue">>, scalar:s(Key)},
%    send(Sock, Peers, {Urn, os:timestamp(), Val}).


%%
%% file descriptor to time series data-base
-spec fd() -> chronolog:fd().

fd() ->
   pipe:ioctl(aura_storage, fd).


