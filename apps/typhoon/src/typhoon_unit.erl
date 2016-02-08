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

-include("typhoon.hrl").

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
   random:seed(os:timestamp()),
   Scenario = scenario:compile(Spec),
   pipe:ioctl_(self(), {trap, true}),
   %% @todo: make singleton udp socket
   {ok, Udp} = gen_udp:open(0, [{sndbuf, 256 * 1024}]),
   erlang:send(self(), request),
   tempus:timer(scenario:t(Scenario), expired),
   {ok, idle, 
      #{
         scenario => Scenario,
         % pid => trace(Name),
         udp => Udp
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
idle(request, Pipe, #{scenario := Scenario0} = State0) ->
   clue:inc({typhoon, req}),
   {Req, Scenario1} = scenario:eval(Scenario0),
   case request(Req, State0) of
      {active,  State1} ->
         {next_state, active, State1#{scenario => Scenario1}};
      {request, State1} ->
         {next_state, idle, State1#{scenario => Scenario1}}
   end;

idle(expired, _, State) ->
   {stop, normal, State};

idle(_, _Pipe, State) ->
   {next_state, idle, State}.


%%
%%
active({http, _, {Code, _Text, _Head, _Env}}, _, #{urn := Urn}=State) ->
   % enq(aura:encode(Urn, os:timestamp(), {http, status, Code}), State),
   {next_state, active, State};

active({trace, T, Msg}, _, #{urn := Urn} = State) ->
   % enq(aura:encode(Urn, T, Msg), State),
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
%% create socket if it is not exists
socket(Url, #{sock := Sock} = State) ->
   %% @todo: monitor socket process
   case erlang:is_process_alive(Sock) of
      false ->
         socket(Url, maps:remove(sock, State));
      true  ->
         Sock         
   end;

socket(Url, _State) ->
   % knet:socket(Url, [{trace, self()}]).
   knet:socket(Url, []).


%%
%%
request(#{type := thinktime, urn := Urn, t := T}, State) ->
   tempus:timer(T, request),
   {request, State};

request(#{type := protocol,  urn := Urn, packet := List}, State) ->
   clue:inc({typhoon, req}),
   {_Mthd, Url, _Head} = hd(List),
   Sock = socket(Url, State),
   lists:foreach(
      fun(Pack) ->
         pipe:send(Sock, Pack)
      end,
      List
   ),
   {active, State#{urn => Urn, sock => Sock}}.


% request(Sock, #{id := Id, req := Mthd, url := Furl, head := Head, data := Fdata}) ->
%    Uri = uri:new( scalar:s(Furl(?CONFIG_SCRIPT_ALLOWED)) ),
%    pipe:send(Sock, {Mthd, Uri, [{'Transfer-Encoding', <<"chunked">>}|Head]}),
%    pipe:send(Sock, scalar:s(Fdata(?CONFIG_SCRIPT_ALLOWED))),
%    pipe:send(Sock, eof),
%    {active, uri:new(Id)};

% request(Sock, #{id := Id, req := Mthd, url := Furl, head := Head}=R) ->
%    Uri = uri:new( scalar:s(Furl(?CONFIG_SCRIPT_ALLOWED)) ),
%    pipe:send(Sock, {Mthd, Uri, Head}),
%    {active, uri:new(Id)};

% request(_Sock, #{sleep := T})
%  when is_integer(T) ->
%    timer:sleep(T),
%    {request, undefined};

% request(_Sock, #{sleep := T})
%  when is_function(T) ->
%    timer:sleep(scalar:i(T(?CONFIG_SCRIPT_ALLOWED))),
%    {request, undefined}.

%%
%% discover destination nodes for sampled data
trace(Name) ->
   lists:map(
      fun(X) ->
         [_, Host] = binary:split(ek:vnode(node, X), <<$@>>),
         {ok, IP}  = inet_parse:address(scalar:c(Host)),
         IP
      end,
      ambitz:entity(vnode,
         ambitz:lookup(
            ambitz:entity(ring, typhoon,
               ambitz:entity(Name)
            )
         )
      )
   ).

%%
%% enqueue sample data
enq(Pack, #{udp := Udp, pid := Peers}) ->
   lists:foreach(
      fun(Peer) -> 
         aura:send(Udp, Peer, Pack)
      end,
      Peers
   ).
   
% %%
% %% compile json specification to executable object's
% compile(Spec) ->
%    [compile(X, Spec) || X <- pair:x(<<"seq">>, Spec)].

% compile(Req, Spec) ->
%    compile(pair:x(<<"@context">>, Req), pair:x(<<"@type">>, Req), Req, Spec).

% compile(?CONFIG_SCHEMA_HTTP, Mthd, Req, Spec)
%  when Mthd =:= <<"GET">> orelse Mthd =:= <<"DELETE">> ->
%    #{
%       id   => pair:x(<<"@id">>, Req),
%       req  => scalar:a(Mthd),
%       url  => compile_url(pair:x(<<"url">>, Req), Spec),
%       head => compile_head(pair:x(<<"head">>, Req), Spec)
%    };

% compile(?CONFIG_SCHEMA_HTTP, Mthd, Req, Spec)
%  when Mthd =:= <<"PUT">> orelse Mthd =:= <<"POST">> ->
%    #{
%       id   => pair:x(<<"@id">>, Req),
%       req  => scalar:a(Mthd),
%       url  => compile_url(pair:x(<<"url">>, Req), Spec),
%       head => compile_head(pair:x(<<"head">>, Req), Spec),
%       data => compile_data(pair:x(<<"data">>, Req), Spec)
%    };

% compile(?CONFIG_SCHEMA_ACTION, <<"sleep">>, Req, _Spec) ->
%    case pair:x(<<"t">>, Req) of
%       X when is_integer(X) ->
%          #{sleep => X};
%       X when is_binary(X) ->
%          Fun = swirl:f(X),
%          #{sleep => Fun(undefined)}
%    end.

% compile_url(Suffix, Spec) ->
%    Base = pair:lookup(<<"base">>, <<>>, Spec),
%    Fun  = swirl:f(<<Base/binary, Suffix/binary>>),
%    Fun(undefined).

% compile_data(Data, _Spec) ->
%    Fun = swirl:f(Data),
%    Fun(undefined).
   
% compile_head(undefined, Spec) ->
%    compile_head([], Spec);
% compile_head(Head, Spec) ->
%    pair:lookup(<<"head">>, [], Spec) ++ Head.
