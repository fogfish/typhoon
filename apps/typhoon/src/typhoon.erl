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
%%   native interface distributed system stress and load testing tool
-module(typhoon).
-author('dmitry.kolesnikov@zalando.fi').

-export([start/0]).
%%
%% management interface
-export([
   define/2
  ,lookup/1
  ,remove/1
  ,run/1
  ,unit/1
]).
%%
%% data interface
-export([
   fd/0
  ,series/4
  ,series/5
  ,metric/4
  ,status/5
]).
%%
%% script interface
-export([
   uid/0
  ,int/1
  ,pareto/2
  ,ascii/1
  ,text/1
]).

%%
%%
-type(id()     :: binary()).
-type(filter() :: mean | max | {pth, float()}).

%%
%% RnD node start
start() ->
   applib:boot(?MODULE, code:where_is_file("dev.config")).

%%%----------------------------------------------------------------------------   
%%%
%%% management interface
%%%
%%%----------------------------------------------------------------------------   

%%
%% defines load scenario to the cluster, 
%% takes unique name of test scenario and its specification,
%% returns cluster descriptor entity
-spec(define/2 :: (id(), any()) -> any()).

define(Id, Spec) ->
   ambitz:spawn(
      ambitz:entity(ring, typhoon, 
         ambitz:entity(service, {typhoon_scenario, start_link, [Id, Spec]},
            ambitz:entity(Id)
         )
      )
   ).

%%
%% lookup load scenario using it's unique name
%% returns cluster descriptor entity, including scenario specification
-spec(lookup/1 :: (id()) -> any()).

lookup(Id) ->
   ambitz:lookup(
      ambitz:entity(ring, typhoon, ambitz:entity(Id))
   ).

%%
%% remove load scenario from cluster
-spec(remove/1 :: (id()) -> any()).

remove(Id) ->
   ambitz:free(
      ambitz:lookup(
         ambitz:entity(ring, typhoon, ambitz:entity(Id))
      )
   ).

%%
%% run load scenario, the scenario will terminate automatically after timeout
-spec(run/1 :: (id()) -> ok).

run(Id) ->
   Pids = ambitz:entity(service,
      ambitz:whereis(
         ambitz:entity(ring, typhoon, ambitz:entity(Id))
      )
   ),
   pipe:call(lists:nth(random:uniform(length(Pids)), Pids), run).

%%
%% return number of active load units
-spec(unit/1 :: (id()) -> integer()).

unit(Id) ->
   lists:sum(
      lists:map(
         fun(X) -> pipe:ioctl(X, n) end,
         ambitz:entity(service,
            ambitz:whereis(
               ambitz:entity(ring, typhoon, ambitz:entity(Id))
            )
         )
      )
   ).



%%%----------------------------------------------------------------------------   
%%%
%%% data interface
%%%
%%%----------------------------------------------------------------------------   

%%
%% file descriptor to time series data-base
-spec(fd/0 :: () -> chronolog:fd()).

fd() ->
   pipe:ioctl(typhoon_peer, fd).

%%
%% read time series values from load scenario, return list of measured values.
%% The function aggregates values on given interval and applies filter.
-spec(series/4 :: (id(), uri:urn(), integer(), chronolog:range()) -> list()).
-spec(series/5 :: (id(), uri:urn(), filter(), integer(), chronolog:range()) -> list()).
   
series(Id, Urn, Chronon, Range) ->
   series(Id, Urn, mean, Chronon, Range).

series(Id, Urn, Filter, Chronon, Range) ->
   Vnode = ambitz:entity(vnode,
      ambitz:lookup(
         ambitz:entity(ring, typhoon, ambitz:entity(Id))
      )
   ),
   Node  = erlang:node(ek:vnode(peer, lists:nth(random:uniform(length(Vnode)), Vnode))),
   pipe:cast({typhoon_peer, Node}, {series, Urn, Filter, Chronon, Range}).

%%
%% read time series value and counts its arrival rate (used for RPS reports)
-spec(metric/4 :: (id(), uri:urn(), integer(), chronolog:range()) -> list()).

metric(Id, Urn, Chronon, Range) ->
   Vnode = ambitz:entity(vnode,
      ambitz:lookup(
         ambitz:entity(ring, typhoon, ambitz:entity(Id))
      )
   ),
   Node  = erlang:node(ek:vnode(peer, lists:nth(random:uniform(length(Vnode)), Vnode))),
   pipe:cast({typhoon_peer, Node}, {metric, Urn, Chronon, Range}).

%%
%% read time series value and counts http status code
-spec(status/5 :: (id(), uri:urn(), integer(), integer(), chronolog:range()) -> list()).

status(Id, Urn, Code, Chronon, Range) ->
   Vnode = ambitz:entity(vnode,
      ambitz:lookup(
         ambitz:entity(ring, typhoon, ambitz:entity(Id))
      )
   ),
   Node  = erlang:node(ek:vnode(peer, lists:nth(random:uniform(length(Vnode)), Vnode))),
   pipe:cast({typhoon_peer, Node}, {status, Urn, Code, Chronon, Range}).

%%%----------------------------------------------------------------------------   
%%%
%%% script interface
%%%
%%%----------------------------------------------------------------------------   

%%
%% generate globally unique sequential identity
-spec(uid/0 :: () -> binary()).

uid() ->
   bits:btoh(uid:encode(uid:g())).

%%
%% generate uniformly distributed integer
-spec(int/1 :: (integer()) -> binary()).

int(N) ->
   scalar:s(random:uniform(N)).

%%
%% generate random integer using Pareto distribution
-spec(pareto/2 :: (float(), integer()) -> binary()).

pareto(A, N) ->
   scalar:s(rand:pareto(A, N)).

%%
%% generate random ASCII payload of given length, 
%% characters are uniformly distributed
-spec(ascii/1 :: (integer()) -> binary()).

ascii(N) ->
   scalar:s(
      stream:list(
         stream:take(N, ascii())
      )
   ).

ascii() ->
   stream:seed(0,
      fun(Seed) ->
         Head = case random:uniform(3) of
            1 -> $0 + (random:uniform(10) - 1);
            2 -> $a + (random:uniform($z - $a) - 1);
            3 -> $A + (random:uniform($Z - $A) - 1)
         end,
         {Head, Seed}
      end
   ).

%%
%% generate random text using Pareto distributions
-spec(text/1 :: (integer()) -> binary()).

text(N) ->
   scalar:s(
      stream:list(
         stream:take(N, text())
      )
   ).

text() ->
   stream:seed(0,
      fun(Seed) ->
         Head = case rand:pareto(0.4, 27) of
            1 -> $ ;
            X -> $a + X - 1
         end,
         {Head, Seed}
      end
   ).


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   






   
