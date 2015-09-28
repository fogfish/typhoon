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
%%   webapp container
-module(zephyrus_webapp).
-author('dmitry.kolesnikov@zalando.fi').

-export([
   content_provided/1, 
   exists/2,
   'GET'/2
]).

%%
%%
content_provided(_Req) ->
   [{'*', '*'}].

%%
%%
exists(_, {Url, _Heads, Env}) ->
   filelib:is_file(filename(Url, Env)).

%%
%%
'GET'(_, {Url, _Heads, Env}) ->
   Filename   = filename(Url, Env),
   {ok, File} = file:read_file(Filename),
   {ok, 
      [
         {'Content-Type', mime(filename:extension(Filename))}
        ,{'Access-Control-Allow-Origin', <<"*">>}
      ], 
      File
   }.

%%
%%
filename(_Url, Env) ->
   filename:join([htdoc(Env), "index.html"]).

%%
%% 
htdoc(Env) ->
   case opts:val(htdoc, Env) of
      X when is_atom(X) -> 
         case code:priv_dir(X) of
            {error,bad_name} -> 
               filename:join([priv, htdoc]);
            Root -> 
               filename:join([Root, htdoc])
         end;
      X when is_list(X) ->
         X 
   end.

%%
%%
mime([])       -> {text, html};
mime(".html")  -> {text, html};
mime(".txt")   -> {text, plain};
mime(".css")   -> {text, css};
mime(".js")    -> {text, javascript};
mime(".xml")   -> {text, xml};

mime(".json")  -> {application, json};
mime(".png")   -> {image, png};
mime(".jpg")   -> {image, jpeg};
mime(".jpeg")  -> {image, jpeg};
mime(".svg")   -> {image, 'svg+xml'};


mime(".pdf")   -> {application, pdf};

mime(_)        -> {application, 'octet-stream'}.
