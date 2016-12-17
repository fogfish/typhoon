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
%%   example of unit test 
-module(unit).
-compile({parse_transform, monad}).

%%
%% scenario actions
-export([get_local_ip/1, get_client_ip/1, discover/1]).


%%%----------------------------------------------------------------------------   
%%%
%%% actions
%%%
%%%----------------------------------------------------------------------------   

%%
%%
get_local_ip(_) ->
   do([m_http ||
      _ /= new("urn:http:httpbin:local:ip"),
      _ /= url("http://127.0.0.1:8888/ip"),
      _ /= header("Connection", "keep-alive"),
      _ /= get(),
      _ <- assert_local_ip(_),
      return(_)
   ]).

assert_local_ip(X) ->
   do([m_unit || 
      _ /= new(X),
      _ /= code(200),
      _ /= eq([origin], "127.0.0.1"),
      return(_)
   ]).


%%
%%
get_client_ip(_) ->
   do([m_http ||
      _ /= new("urn:http:httpbin:client:ip"),
      _ /= url("http://127.0.0.1:8888/ip"),
      _ /= header("Connection", "keep-alive"),
      _ /= get(),
      _ <- assert_client_ip(_),
      return(_)
   ]).

assert_client_ip(X) ->
   do([m_unit || 
      _ /= new(X),
      _ /= code(404),
      _ /= ne([origin], "127.0.0.1"),
      return(_)
   ]).

%%
%%
discover(_) ->
   do([m_http ||
      _ /= new("urn:http:cs:mobile:brand"),
      _ /= url( q() ),
      _ /= header("Connection", "keep-alive"),
      _ /= get(),
      _ <- assert_discover(_),
      return(_)
   ]).

q() ->
   uri:q(
      [
         {uuid, "D0E545F4-E2D2-4802-933D-8659DAAD464E"},
         {se, ""},
         {page, 1},
         {ts, tempus:m(os:timestamp())},
         {appVersion, "4.4.0"},
         {perPage, 24},
         {deviceType, "phone"},
         {sig, "c3885e8ac73ccdfe0c9383749e3e25c9579dc479"},
         {season, ""},
         {search_use_case, categories},
         {dir, desc},
         {order, popularity},
         {fields, "products"},
         {screenDensity, 200},
         {devicePlatform, ios},
         {appdomainId, 19}
      ],
      uri:path("/catalog-and-search", uri:new("https://mango-live.poirot.zalan.do"))
   ).

assert_discover(X) ->
   do([m_unit || 
      _ /= new(X),
      _ /= code(200),
      _ /= gt([totalPages], 0),
      _ /= has([products]),
      _ /= has([products, {uniform}, sku]),
      _ /= has([aaa]),
      _ /= has([filters, {type, assortment_area}, 1, label]),
      return(_)
   ]).

