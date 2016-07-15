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

-export([start/0]).
-export([c/2]).
%% script definition interface
-export([
   new/1,
   method/2,
   url/2,
   header/3,
   payload/2,
   request/2,
   request/1,
   thinktime/2
]).
%% script utility interface
-export([
   join/1,
   uid/0,
   uniform/1,
   pareto/2,
   ascii/1,
   text/1,
   json/1
]).

%%
%% start application at development console 
start() ->
   applib:boot(?MODULE, []).

%%
%% compiles scenario file
-spec c(atom(), string()) -> {ok, atom(), binary()} | {error, _, _}.

c(Id, File) ->
   case cc_ast(File) of
      {ok, _, Forms} ->
         cc_obj(cc_mod(Id, Forms));
      Error ->
         Error
   end.

%%
%% compile scenario file to abstract syntax tree
cc_ast(File) ->
   compile:file(File, 
      ['P', return_errors, binary, {parse_transform, monad}, no_error_module_mismatch]
   ).

%%
%% compile scenario to object code
cc_obj(Code) ->
   compile:forms(Code, 
      [return_errors, binary, {parse_transform, monad}]
   ).  

%%
%% rename module
cc_mod(Id, [{attribute, Ln, module, _}|Tail]) ->
   [{attribute, Ln, module, Id} | cc_mod(Id, Tail)];
cc_mod(Id, [Head|Tail]) ->
   [Head | cc_mod(Id, Tail)];
cc_mod(_, []) ->
   [].


%%%----------------------------------------------------------------------------   
%%%
%%% http protocol
%%%
%%%----------------------------------------------------------------------------   

%%
%% 
-spec new(string()) -> _.

new({urn, <<"http">>, _} = Id) ->
   #{
      id     => Id, 
      method => 'GET', 
      header => []
   };

new(Id) ->
   new( uri:new(scalar:s(Id)) ).

%%
%% 
-spec method(atom() | string() | binary(), _) -> _.

method(Method, #{id := {urn, <<"http">>, _}} = Http) ->
   % @todo: validate and normalize method
   Http#{method => Method}.

%%
%% 
-spec url(string() | binary(), _) -> _.

url(Url, #{id := {urn, <<"http">>, _}} = Http) ->
   Http#{url => uri:new(Url)}.

%%
%% 
-spec header(string(), string(), _) -> _.
 
header(Head, Value, #{id := {urn, <<"http">>, _}, header := List} = Http) -> 
   % @todo: fix htstream to accept various headers
   Http#{header => [{scalar:atom(Head), scalar:s(Value)} | List]}.

%%
%% 
-spec payload(string() | binary(), _) -> _.

payload(Pckt, #{id := {urn, <<"http">>, _}, header := List} = Http) ->
   Http#{payload => Pckt, header => [{'Transfer-Encoding', <<"chunked">>} | List]}.

%%
%% 
-spec request(_) -> _.
-spec request([atom()], _) -> _.

request(Ln, #{id := {urn, <<"http">>, _}} = Http) ->
   fun(IO) ->
      Pckt = send(IO, Http),
      Lens = lens(Ln),
      lens:get(Lens, jsx:decode(Pckt))
   end.

request(#{id := {urn, <<"http">>, _}} = Http) ->
   fun(IO) -> 
      send(IO, Http)
   end.

%%
%%
-spec thinktime(_, integer()) -> _.

thinktime(X, T) ->
   fun(_IO) ->
      timer:sleep(T),
      X
   end.

%%
%%
lens(Ln)
 when is_list(Ln) ->
   lens:c([lens(X) || X <- Ln]);

lens(Ln)
 when is_atom(Ln) ->
   lens:pair(scalar:s(Ln), []);

lens({uniform}) ->
   fun
   (Fun,   []) ->
      lens:fmap(fun(_) -> [] end, Fun([]));
   (Fun, List) ->
      Value = lists:nth(random:uniform(length(List)), List),
      lens:fmap(fun(_) -> List end, Fun(Value))
   end;

lens({pareto, A}) ->
   fun
   (Fun,   []) ->
      lens:fmap(fun(_) -> [] end, Fun([]));
   (Fun, List) ->
      Value = lists:nth(pdf:pareto(A, length(List)), List),
      lens:fmap(fun(_) -> List end, Fun(Value))
   end.

%%%----------------------------------------------------------------------------   
%%%
%%% script interface
%%%
%%%----------------------------------------------------------------------------   

%%
%% join terms to string
-spec join([_]) -> binary().

join(Terms) ->
   scalar:ls([scalar:ls(X) || X <- Terms]).

%%
%% generate globally unique sequential (k-order) identity
-spec uid() -> binary().

uid() ->
   bits:btoh( uid:encode(uid:g()) ).

%%
%% generate uniformly distributed integer or term
-spec uniform(integer() | [_]) -> binary().

uniform(N)
 when is_integer(N) ->
   scalar:s(random:uniform(N));
uniform(List)
 when is_list(List) ->
   scalar:ls(
      lists:nth(
         random:uniform(length(List)), 
         List
      )
   ).

%%
%% generate random integer or term using Pareto distribution
-spec pareto(float(), integer() | [_]) -> binary().

pareto(A, N)
 when is_integer(N) ->
   scalar:s(pdf:pareto(A, N));
pareto(A, List)
 when is_list(List) ->
   scalar:ls(
      lists:nth(
         pdf:pareto(A, length(List)),
         List
      )
   ).

%%
%% generate random ASCII payload of given length, 
%% characters are uniformly distributed
-spec ascii(integer()) -> binary().

ascii(N) ->
   scalar:s(
      stream:list(
         stream:take(N, ascii())
      )
   ).

ascii() ->
   stream:unfold(
      fun(Seed) ->
         Head = case random:uniform(3) of
            1 -> $0 + (random:uniform(10) - 1);
            2 -> $a + (random:uniform($z - $a) - 1);
            3 -> $A + (random:uniform($Z - $A) - 1)
         end,
         {Head, Seed}
      end,
      0
   ).

%%
%% generate random text using Pareto distributions
-spec text(integer()) -> binary().

text(N) ->
   scalar:s(
      stream:list(
         stream:take(N, text())
      )
   ).

text() ->
   stream:unfold(
      fun(Seed) ->
         Head = case pdf:pareto(0.4, 27) of
            1 -> $ ;
            X -> $a + X - 1
         end,
         {Head, Seed}
      end,
      0
   ).

%%
%% converts list of tuples into json
-spec json([{atom, _}]) -> binary().

json(Json) ->
   jsx:encode(Json).


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% send request
send(#{pool := Pool, peer := Peer}, #{id := {urn, <<"http">>, _} = Urn, url := Url, header := Head} = Http) ->
   NetIO = Pool(Url, Head),
   {ok, Sock} = pq:lease( NetIO ),
   Pckt = pipe:call(Sock, {request, Urn, Peer, encode(Http)}, 30000),
   pq:release(NetIO, Sock),
   Pckt.


%%
%% build request
encode(#{id := {urn, <<"http">>, _}} = Http) ->
   lists:flatten([
      encode_http_req(Http), 
      encode_http_entity(Http), 
      encode_http_eof(Http)
   ]).

encode_http_req(#{method := Mthd, url := Url, header := Head}) ->
   [{Mthd, Url, Head}].

encode_http_entity(#{payload := Payload}) ->
   [scalar:s(Payload)];
encode_http_entity(_) ->
   [].

encode_http_eof(_) ->
   [eof].

