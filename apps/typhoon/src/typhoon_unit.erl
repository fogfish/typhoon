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
%%   load unit - generates load towards SUT
-module(typhoon_unit).
-behaviour(pipe).
-author('dmitry.kolesnikov@zalando.fi').

-export([
   start_link/2
  ,init/1
  ,free/2
  ,idle/3
  ,active/3
]).

%%-----------------------------------------------------------------------------
%%
%% factory
%%
%%-----------------------------------------------------------------------------

start_link(Name, Spec) ->
   pipe:start_link(?MODULE, [Name, Spec], []).

init([Name, Spec]) ->
   pipe:ioctl_(self(), {trap, true}),
   erlang:send(self(), request),
   tempus:timer(tempus:t(s, pair:x(<<"t">>, Spec)), expired),
   random:seed(os:timestamp()),
   {ok, Udp} = gen_udp:open(0, [{sndbuf, 256 * 1024}]),
   {ok, idle, 
      #{
         seq  => q:new(pair:x(<<"seq">>, Spec)),
         pid  => trace(Name),
         udp  => Udp
      }
   }.

free(_Reason, #{udp := Udp}) ->
   gen_udp:close(Udp).

%%-----------------------------------------------------------------------------
%%
%% pipe
%%
%%-----------------------------------------------------------------------------

%%
%%
idle(request, Pipe, #{sock := Sock, seq := Seq} = State) ->
   case erlang:is_process_alive(Sock) of
      false ->
         idle(request, Pipe, maps:remove(sock, State));
      true  ->
         {next_state, active, 
            State#{
               urn => request(Sock, q:head(Seq)),
               seq => q:enq(q:head(Seq), q:tail(Seq))
            }
         }
   end;
         
idle(request, Pipe, #{seq := Seq}=State) ->
   idle(request, Pipe, State#{sock => socket(q:head(Seq))});

idle(expired, _, State) ->
   {stop, normal, State};

idle(_, _Pipe, State) ->
   {next_state, idle, State}.


%%
%%
active({http, _, {Code, _Text, _Head, _Env}}, _, #{udp := Udp, pid := Pid, urn := Urn}=State) ->
   T = os:timestamp(),
   lists:foreach(
      fun(Peer) ->
         aura:send(Udp, Peer, 
            aura:encode(Urn, T, {http, status, Code})
         )
         % Port = 10020 + random:uniform(10) - 1,
         % Pack = erlang:term_to_binary({trace, Urn, T, {http, status, Code}}),
         % gen_udp:send(Udp, X, Port, <<"typhoon:", (base64:encode(Pack))/binary>>)
         % pipe:send(X, {trace, Urn, T, {http, status, Code}})
      end,
      Pid
   ),
   {next_state, active, State};

active({trace, T, Msg}, _, #{udp := Udp, pid := Pid, urn := Urn} = State) ->
   lists:foreach(
      fun(Peer) -> 
         aura:send(Udp, Peer, 
            aura:encode(Urn, T, Msg)
         )
         % Port = 10020 + random:uniform(10) - 1,
         % Pack = erlang:term_to_binary({trace, Urn, T, Msg}),
         % gen_udp:send(Udp, X, Port, <<"typhoon:", (base64:encode(Pack))/binary>>)
         % % pipe:send(X, {trace, Urn, T, Msg})
      end,
      Pid
   ),
   {next_state, active, State};

active({http, _, eof}, _, State) ->
   erlang:send(self(), request),
   {next_state, idle, State};

active({http, _, _Pckt}, _, State) ->
   {next_state, active, State};

active(expired, _, State) ->
   {stop, normal, State};

active(_, _Pipe, State) ->
   {next_state, active, State}.

%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
%% create socket
socket(Req) ->
   knet:socket(uri:new(pair:x(<<"url">>, Req)), [{trace, self()}]).

%%
%%
request(Sock, Req) ->
   request(Sock, 
      pair:x(<<"id">>,   Req)
     ,pair:x(<<"req">>,  Req)
     ,pair:x(<<"url">>,  Req)
     ,pair:x(<<"head">>, Req)
     ,pair:x(<<"data">>, Req)
   ).


request(Sock, Id, Mthd, Url, Head, undefined) ->
   Uri = uri:new(scalar:s(swirl:r(scalar:c(Url), []))),
   pipe:send(Sock, {scalar:a(Mthd), Uri, Head}),
   uri:new(Id);

request(Sock, Id, Mthd, Url, Head, Data) ->
   Uri = uri:new(scalar:s(swirl:r(scalar:c(Url), []))),
   pipe:send(Sock, {scalar:a(Mthd), Uri, [{'Transfer-Encoding', <<"chunked">>}|Head]}),
   pipe:send(Sock, scalar:s(swirl:r(scalar:c(Data), []))),
   pipe:send(Sock, eof),
   uri:new(Id).

%%
%% discover trace end-point
% trace(Name) ->
%    ambitz:entity(service,
%       ambitz:whereis(
%          ambitz:entity(ring, typhoon,
%             ambitz:entity(Name)
%          )
%       )
%    ).

trace(Name) ->
   lists:map(
      fun(X) ->
         [_, Host] = binary:split(ek:vnode(node, X), <<$@>>),
         scalar:c(Host)
      end,
      ambitz:entity(vnode,
         ambitz:lookup(
            ambitz:entity(ring, typhoon,
               ambitz:entity(Name)
            )
         )
      )
   ).



