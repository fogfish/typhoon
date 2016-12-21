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
%%   rest api - scenario management
-module(zephyrus_editor_create).
-author('ivan.yurchenko@zalando.fi').

-export([
   allowed_methods/1,
   content_provided/1, 
   'GET'/3
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{text, html}].

%%
%%
'GET'(_Type, _Msg, _) ->
   html(<<"create-scenario">>).

html(File) ->
   file:read_file(
      filename:join([code:priv_dir(zephyrus), htdoc, <<File/binary, ".html">>])
   ).
