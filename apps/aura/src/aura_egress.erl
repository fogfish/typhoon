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
%%   telemetry egress queue 
%%    * defines pool of re-usable datagram socket 
%%    * serialize telemetry message to wire format
-module(aura_egress).
-behaviour(pipe).

-export([
   start_link/0,
   init/1,
   free/2,
   handle/3
]).

%% udp port base
-define(PORT,  20100).


start_link() ->
   pipe:start_link(?MODULE, [], []).


init([]) ->
   %% @todo: make buffer configurable
   {ok, Sock} = gen_udp:open(0, [{sndbuf, 1 * 1024 * 1024}]),
   {ok, handle, Sock}.


free(_, Sock) ->
   gen_udp:close(Sock).


handle({Peers, {Urn, T, Telemetry}}, _Pipe, Sock) ->
   Pack = aura_protocol:encode(Urn, T, Telemetry),
   Port = ?PORT + random:uniform(10) - 1,
   lists:foreach(
      fun(Peer) -> 
         gen_udp:send(Sock, Peer, Port, Pack)
      end,
      Peers
   ),
   clue:inc({aura, egress}, length(Peers)),
   {next_state, handle, Sock}.
   



