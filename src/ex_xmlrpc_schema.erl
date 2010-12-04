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

%% @private
-module(ex_xmlrpc_schema).
-author('Anthony Ramine <nox@dev-extend.eu>').
-behaviour(gen_server).
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("xmerl/include/xmerl_xsd.hrl").

-export([start_link/0,
         validate_response/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%% @spec start_link() -> {ok, pid()} | ex_xmlrpc:start_error()
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{timeout, infinity}]).

%% @spec validate_response(#xmlElement{}) -> ok | {error, term()}
validate_response(Element) ->
  gen_server:call(?MODULE, {validate_response, Element}, infinity).


%% @hidden
init([]) ->
  Schema = filename:join([code:priv_dir(ex_xmlrpc), "xmlrpc-response.xsd"]),
  xmerl_xsd:process_schema(Schema).

%% @hidden
handle_call({validate_response, Element}, _From, State) ->
  case xmerl_xsd:validate(Element, State) of
    Error = {error, _Reasons} -> {reply, Error, State};
    {Response, _State} -> {reply, check_response(Response, State), State} end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @hidden
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
  {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
  ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @spec check_response(#xmlElement{}, #xsd_state{}) -> ok | {error, term()}
check_response(Response = #xmlElement{name = methodResponse}, State) ->
  Params = ex_xmlrpc_lib:get_child_element(Response),
  Param = ex_xmlrpc_lib:get_child_element(Params),
  Value = ex_xmlrpc_lib:get_child_element(Param),
  check_value(Value, State);
check_response(Element, _State) ->
  {error, {bad_response_root, Element}}.

%% @spec check_value(#xmlElement{}, #xsd_state{}) -> ok | {error, term()}
check_value(#xmlElement{content = [#xmlText{}]}, _State) ->
  ok;
check_value(Element, State) ->
  case xmerl_xsd:validate(Element, State) of
    Error = {error, _Reasons} -> Error;
    {Value, _State} ->
      case Child = ex_xmlrpc_lib:get_child_element(Value) of
        #xmlElement{name = array} -> check_array(Child, State);
        #xmlElement{name = struct} -> check_struct(Child, State);
        _ -> ok end end.

%% @spec check_array(#xmlElement{}, #xsd_state{}) -> ok | {error, term()}
check_array(Element, State) ->
  #xmlElement{content = Content} = ex_xmlrpc_lib:get_child_element(Element),
  check_values([ E || E = #xmlElement{} <- Content ], State).

%% @spec check_struct(#xmlElement{}, #xsd_state{}) -> ok | {error, term()}
check_struct(Element, State) ->
  check_values(xmerl_xpath:string("/member/value", Element), State).

%% @spec check_values([#xmlElement{}], #xsd_state{}) -> ok | {error, term()}
check_values([Value | Values], State) ->
  case check_value(Value, State) of
    ok -> check_values(Values, State);
    Error -> Error end;
check_values([], _State) ->
  ok.
