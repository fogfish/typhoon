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
-module(aura_app).
-behaviour(application).
-author('dmitry.kolesnikov@zalando.fi').


-export([
   start/2
  ,stop/1
]).

%%
%%
start(_Type, _Args) ->
   {ok,   _} = kmq:queue(auraq, [
      opts:get(n,   1, aura),
      opts:get(in, [], aura),
      opts:get(mq, [], aura)
   ]),
   clue:define(meter, {aura, io}, 600000),
   aura_sup:start_link().

%%
%%
stop(_State) ->
   ok.

