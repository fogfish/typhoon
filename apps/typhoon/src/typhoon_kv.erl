-module(typhoon_kv).
-include_lib("ambitz/include/ambitz.hrl").
-compile({parse_transform, category}).

-export([
   lookup/3
  ,append/4
  ,remove/4
]).

-type ns()   :: atom().
-type key()  :: uri:urn().
-type val()  :: _.
-type opts() :: _.

%%
%%
-spec lookup(ns(), key(), opts()) -> {ok, [val()]} | {error, _}.

lookup(Ns, Key, Opts) ->
   [$^|| 
      dlookup(Ns, Key, Opts), 
      value(_)
   ].

dlookup(Ns, Key, Opts) ->
  case ambitz:get(typhoon, uri:s(Key), Ns, Opts) of
      {ok, #entity{val = undefined}} ->
         {error, not_found};
      {ok, #entity{val = Set}} ->
         {ok, Set}
   end.

%%
%%
-spec append(ns(), key(), val(), opts()) -> {ok, [val()]} | {error, _}.

append(Ns, Key, Val, Opts) ->
   [$^||
      orsets_add(Val),
      dappend(Ns, Key, _, Opts),
      value(_)
   ].

dappend(Ns, Key, Val, Opts) ->
   case ambitz:put(typhoon, uri:s(Key), Ns, Val, Opts) of
      {ok, #entity{val = undefined}} ->
         {error, not_found};
      {ok, #entity{val = Set}} ->
         {ok, Set}
   end.

%%
%%
-spec remove(ns(), key(), val(), opts()) -> {ok, [val()]} | {error, _}.

remove(Ns, Key, Val, Opts) ->
   [$^||
      dlookup(Ns, Key, Opts),
      orsets_sub(Val, _),
      dappend(Ns, Key, _, Opts),
      value(_)
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

orsets_add(Val) ->
   {ok, crdts:update(add, Val, crdts:new(orsets))}.

orsets_sub(Val, Set) ->
   {ok, crdts:update(sub, Val, Set)}.

value(Set) ->
   {ok, crdts:value(Set)}.
