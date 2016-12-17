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
%%   experimental unit test monad
-module(m_unit).

-export([return/1, fail/1, '>>='/2]).
-export([new/1, eq/2, ne/2, le/2, lt/2, ge/2, gt/2, has/1, code/1, head/2]).

%%%----------------------------------------------------------------------------   
%%%
%%% state monad
%%%
%%%----------------------------------------------------------------------------   

return(_) -> 
   fun(State) -> 
      Url  = lens:get(url(), State),
      Unit = lens:get(unit(), State),
      [#{id => uri:s(Url), unit => lists:reverse(Unit)}|maps:remove(unit, State)] 
   end.

fail(X) ->
   m_state:fail(X).

'>>='(X, Fun) ->
   m_state:'>>='(X, Fun).


%%%----------------------------------------------------------------------------   
%%%
%%% asserts
%%%
%%%----------------------------------------------------------------------------   

%%
%%
url()  -> lens:c([lens:map(fd, #{}), lens:map(url,  none)]).
code() -> lens:c([lens:map(unit, #{}), lens:map(code,  none)]).
head() -> lens:c([lens:map(unit, #{}), lens:map(head,    [])]).
data() -> lens:c([lens:map(unit, #{}), lens:map(data,  none)]).
unit() -> lens:c([lens:map(unit, #{}), lens:map(unit,    [])]).

%%
%%
new([{Code, _, Head, _} | Payload]) ->
   fun(State0) ->
      Mime   = lens:get(lens:pair('Content-Type'), Head),
      Data   = decode(Mime, erlang:iolist_to_binary(Payload)),
      State1 = lens:put(code(), Code, State0),
      State2 = lens:put(head(), Head, State1),
      State3 = lens:put(data(), Data, State2),
      [Data|State3]      
   end.

decode({_, <<"json">>}, Payload) ->
   jsx:decode(erlang:iolist_to_binary(Payload));

decode(_, Payload) ->
   erlang:iolist_to_binary(Payload).

%%
%%
eq(Lens, Value) -> check(eq, fun(A, B) -> A =:= B end, Lens, Value).
ne(Lens, Value) -> check(ne, fun(A, B) -> A =/= B end, Lens, Value).
le(Lens, Value) -> check(le, fun(A, B) -> A =<  B end, Lens, Value).
lt(Lens, Value) -> check(lt, fun(A, B) -> A  <  B end, Lens, Value).
ge(Lens, Value) -> check(ge, fun(A, B) -> A  >= B end, Lens, Value).
gt(Lens, Value) -> check(gt, fun(A, B) -> A  >  B end, Lens, Value).

check(Check, Fun, Lens, Value) ->
   fun(State) ->
      Expect = value(Value),
      Actual = scenario:lens(Lens, lens:get(data(), State)),
      Units  = lens:get(unit(), State),
      Unit   = #{check => Check, pass => Fun(Actual, Expect), lens => Lens, expect => Expect, actual => Actual},
      [ok|lens:put(unit(), [Unit|Units], State)]      
   end.

%%
%%
has(Lens) ->
   fun(State) ->
      Actual = scenario:lens(Lens, lens:get(data(), State)),
      Units  = lens:get(unit(), State),
      Unit   = #{check => has, pass => Actual =/= [], lens => Lens, expect => property, actual => Actual =/= []},
      [ok|lens:put(unit(), [Unit|Units], State)]      
   end.

%%
%%
code(Expect) ->
   fun(State) ->
      Actual = lens:get(code(), State),
      Units  = lens:get(unit(), State),
      Unit   = #{check => code, pass => Expect =:= Actual, expect => Expect, actual => Actual},
      [ok|lens:put(unit(), [Unit|Units], State)]      
   end.

%%
%%
head(Head, Value) ->
   fun(State) ->
      Expect = value(Value),
      Actual = lens:get(lens:pair(scalar:atom(Head), <<>>), lens:get(head(), State)),
      Units  = lens:get(unit(), State),
      Unit   = #{check => head, pass => Expect =:= Actual, expect => Expect, actual => Actual},
      [ok|lens:put(unit(), [Unit|Units], State)]      
   end.
   
%%
%%
value(X)
 when is_list(X) ->
   scalar:s(X);
value(X) ->
   X.

