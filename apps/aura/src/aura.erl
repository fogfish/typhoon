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
   fd/0
  ,send/3
  ,clue/1
  ,stream/2
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
%% file descriptor to time series data-base
-spec fd() -> chronolog:fd().

fd() ->
   pipe:ioctl(aura_storage, fd).

%%
%% send telemetry for processing
-spec send(uri:urn(), tempus:t(), number()) -> ok.


send(Urn, T, {_, _, _} = X) ->
   pts:send(aura_stream, Urn, {T, tempus:u(X)});
send(Urn, T, X) ->
   pts:send(aura_stream, Urn, {T, X}).

%%
%% 
-spec clue(uri:urn()) -> ok.

clue(Urn) ->
   %% @todo: think about ambit instead of pure rpc
   Node = hd(peer(Urn)),
   rpc:call(Node, pts, get, [aura_sensor, Urn]).

%%
%%
-spec stream(uri:urn(), _) -> [{_, _}].

stream(Urn, Fun) ->
   %% @todo: think about ambit instead of pure rpc
   Node = hd(peer(Urn)),
   pipe:call({aura_storage, Node}, {stream, Fun}, 300000).   


peer(Urn) ->
   [erlang:node(ek:vnode(peer, Vnode)) 
      || Vnode <- ek:successors(aura, uri:s(Urn)), 
         ek:vnode(type, Vnode) == primary].




