%% Copyright (c) 2010, Dev:Extend
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%  * Redistributions of source code must retain the above copyright notice,
%%    this list of conditions and the following disclaimer.
%%  * Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%  * Neither the name of Dev:Extend nor the names of its contributors may be
%%    used to endorse or promote products derived from this software without
%%    specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%% @type start_error() = {error, {already_started, pid()} | term()}.
%% @type server_ref() = Name::atom() | {Name::atom(), Node::atom()} | pid().
%% @type value() = int() | boolean() | float() | binary() | array() | struct().
%% @type array() = [value()].
%% @type struct() = [{}] | [{name(), value()}].
%% @type name() = atom() | string() | binary().
%% @type fault() = {fault, int(), binary()}.

%% @doc The XML-RPC client.
-module(ex_xmlrpc).
-author('Anthony Ramine <nox@dev-extend.eu>').
-behaviour(gen_server).

-export([start/2, start/3, start/4,
         start_link/4,
         stop/1,
         call/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% @equiv start(Name, URL, [])
start(Name, URL) ->
  start(Name, URL, []).

%% @equiv start(Name, URL, ClientOptions, [])
start(Name, URL, ClientOptions) ->
  start(Name, URL, ClientOptions, []).

%% @spec start(atom(), string(), list(), list()) -> {ok, pid()} | start_error()
%% @doc Start a new XML-RPC client.
%%      The name of the client is used as the inets client profile.
%%      `ClientOptions' is passed after the inets HTTP client initialization
%%      to `httpc:set_options/2' and `HTTPOptions' at each remote procedure
%%      call to `httpc:request/5'.
start(Name, URL, ClientOptions, HTTPOptions) ->
  ex_xmlrpc_sup:start_client(Name, URL, ClientOptions, HTTPOptions).

%% @spec start_link(atom(), string(), list(), list()) -> {ok, pid()} |
%%                                                       start_error()
%% @doc Start a new XML-RPC client. The client is linked to the calling
%%      process.
%% @see start/4
start_link(Name, URL, ClientOptions, HTTPOptions) ->
  gen_server:start_link({local, Name}, ?MODULE,
                        {Name, URL, ClientOptions, HTTPOptions},
                        [{timeout, infinity}]).

%% @spec stop(server_ref()) -> ok
%% @doc Stop a client.
stop(ServerRef) ->
  gen_server:cast(ServerRef, stop).

%% @spec call(server_ref(), name(), [value()]) -> value() | fault() | error()
%%       where error() = {error, term()}
%% @doc Call a remote procedure.
call(ServerRef, Method, Params) ->
  gen_server:call(ServerRef, {call, Method, Params}, infinity).


%% @hidden
init({Profile, URL, ClientOptions, HTTPOptions}) ->
  {ok, _Pid} = inets:start(httpc, [{profile, Profile}]),
  ok = httpc:set_options(ClientOptions, Profile),
  {ok, {Profile, URL, HTTPOptions}}.

%% @hidden
handle_call({call, Method, Params}, _From,
            State = {Profile, URL, HTTPOptions}) ->
  Body = ex_xmlrpc_lib:call_to_binary(Method, Params),
  Request = {URL, [], "text/xml", Body},
  case httpc:request(post, Request, [], HTTPOptions, Profile) of
    {ok, {{_HTTPVersion, StatusCode, ReasonPhrase}, _Headers, ResponseBody}} ->
      case StatusCode of
        200 -> {reply, handle_body(ResponseBody), State};
        _ -> {reply, {error, {StatusCode, ReasonPhrase}}, State} end;
    {error, Reason} -> {reply, {error, {httpc, Reason}}, State} end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @hidden
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
  {noreply, State}.

%% @hidden
terminate(_Reason, {Profile, _URL, _HTTPOptions}) ->
  inets:stop(httpc, Profile).

%% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% @spec handle_body(string()) -> value() | {error, term()}
handle_body(ResponseBody) ->
  case catch xmerl_scan:string(ResponseBody) of
    {'EXIT', Term} -> {error, {bad_xml, Term}};
    {Element, _Rest} -> handle_xml(Element) end.

%% @spec handle_xml(#xmlElement{}) -> value() | {error, term()}
handle_xml(Element) ->
  case ex_xmlrpc_schema:validate_response(Element) of
    {error, Reason} -> {error, {invalid_response, Reason}};
    ok -> ex_xmlrpc_lib:response_to_value(Element) end.
