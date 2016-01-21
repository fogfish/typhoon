%%
%%   Copyright 2016 Zalando SE
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
%%   the library implements definition, and compilation of typhoon load scripts.
%%   see docs/scenario.md for detailed specification of dsl
-module(scenario).
-include("scenario.hrl").

-export([
   compile/1,
   eval/1
]).

%%
-type scenario() :: datum:q().

%%
%% compile scenario
-spec compile(_) -> scenario().

compile(Spec) ->
   q:new([compile_req(Spec, Req) 
      || Req <- lens:get(lens:pair(<<"seq">>), Spec)]).

%%
%% evaluates request to list of communication primitives
-spec eval(fun((_) -> ok), scenario()) -> {[_], scenario()}.

eval(Scenario) ->
   eval(q:head(Scenario), q:enq(q:head(Scenario), q:tail(Scenario))).

eval(Unit, Scenario) ->
   List = lists:flatten([
      http_head(Unit), http_payload(Unit), http_eof(Unit)
   ]),
   {List, Scenario}.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% compile request
compile_req(Spec, Unit) ->
   compile_thinktime(Spec, Unit,
      compile_payload(Spec, Unit,
         compile_uri(Spec, Unit, 
            compile_header(Spec, Unit, 
               compile_method(Spec, Unit, #{})
            )
         )
      )
   ).
   
%%
compile_uri(Spec, Unit, Req) ->
   Base = lens:get(lens:pair(<<"url">>, <<>>), Spec),
   Path = lens:get(lens:pair(<<"url">>), Unit),
   Fun  = swirl:f(<<Base/binary, Path/binary>>),
   Req#{uri => Fun(undefined)}.

%%
compile_payload(_Spec, Unit, Req) ->
   case lens:get(lens:pair(<<"payload">>, undefined), Unit) of
      undefined ->
         Req;
      Data ->
         Fun = swirl:f(Data),
         Req#{payload => Fun(undefined)}
   end.

%%
compile_header(Spec, Unit, Req) ->
   Base = lens:get(lens:pair(<<"header">>, []), Spec),
   Head = lens:get(lens:pair(<<"header">>, []), Unit),
   Req#{header => Base ++ Head}.

%%
compile_method(_Spec, Unit, Req) ->
   Mthd = lens:get(lens:pair(<<"method">>, []), Unit),
   Req#{method => scalar:a(Mthd)}.

%%
compile_thinktime(_Spec, Unit, Req) ->
   case lens:get(lens:pair(<<"thinktime">>, undefined), Unit) of
      undefined ->
         Req;
      T when is_integer(T) ->
         Req#{thinktime => fun(_) -> T end};
      T when is_binary(T) ->
         Fun = swirl:f(T),
         Req#{payload => Fun(undefined)}
   end.


%%
%%
http_head(#{method := Mthd, uri := Uri, header := Head}) ->
   [
      {
         Mthd, 
         uri:new(scalar:s( Uri(?CONFIG_SCRIPT_ALLOWED) )), 
         [{'Transfer-Encoding', <<"chunked">>}|Head]
      }
   ].

%%
http_payload(#{payload := Payload}) ->
   [
      scalar:s( Payload(?CONFIG_SCRIPT_ALLOWED) )
   ];
http_payload(_) ->
   [].

%%
http_eof(_) ->
   [eof].

