%% @doc
%%   pipe complaint adapter of knet library
-module(njord_adapter).
-behaviour(pipe).

-export([
   start_link/2
  ,start_link/1
  ,init/1
  ,free/2
  ,handle/3
]).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

start_link(Uri) ->
   start_link(Uri, []).

start_link(Uri, Opts) ->
   pipe:start_link(?MODULE, [Uri, Opts], []).

init([Uri, Opts]) ->
   {ok, handle,
      #{
         sock => knet:socket(Uri, [{trace, self()}|Opts])
      }
   }.

free(_Reason, #{sock := Sock}) ->
   knet:close(Sock).

%%%----------------------------------------------------------------------------   
%%%
%%% pipe
%%%
%%%----------------------------------------------------------------------------   

% handle({lease, Urn}, _Pipe, #{sock := _Sock} = State) ->
%    {next_state, handle, State};

% handle({lease, Urn}, Pipe, State) ->
%    % lease 
%    {}

handle({trace, T, Msg}, _Pipe, State) ->
   % socket trace message
   io:format("=[ tr ]=> ~p : ~p~n", [T, Msg]),
   {next_state, handle, State};   

handle({Prot, Sock, Ingress}, Pipe, #{sock := Sock} = State) ->
   % ingress message from network 
   pipe:b(Pipe, {Prot, self(), Ingress}),
   {next_state, handle, State};

handle(Egress, Pipe, State) ->
   pipe:b(Pipe, Egress),
   {next_state, handle, State}.


