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

%% @type iodata() = iolist() | binary().
%% @type iolist() = [char() | string() | binary()].

%% @private
-module(ex_xmlrpc_lib).
-author('Anthony Ramine <nox@dev-extend.eu>').
-include_lib("xmerl/include/xmerl.hrl").

-export([call_to_binary/2,
         response_to_value/1,
         get_child_element/1]).


%% @spec call_to_binary(ex_xmlrpc:name(), [ex_xmlrpc:value()]) -> binary()
call_to_binary(Method, Params) ->
  ParamsList = case Params of
                 [] -> [];
                 _ -> params_to_iolist(Params) end,
  iolist_to_binary([<<"<methodCall>">>,
                   name_to_iolist(Method, <<"methodName">>),
                   ParamsList,
                   <<"</methodCall>">>]).

%% @spec response_to_value(#xmlElement{}) -> ex_xmlrpc:value()
response_to_value(Response) ->
  Params = get_child_element(Response),
  Param = get_child_element(Params),
  Value = get_child_element(Param),
  element_to_value(Value).

%% @spec get_child_element(Element::#xmlElement{}) -> #xmlElement{}
get_child_element(#xmlElement{content = Content}) ->
  get_child_element2(Content).
%% @hidden
get_child_element2([#xmlText{} | Content]) ->
  get_child_element2(Content);
get_child_element2([Child = #xmlElement{} | _Content]) ->
  Child.


%% @spec name_to_iolist(ex_xmlrpc:name(), iodata()) -> iolist()
name_to_iolist(Name, Tag) when is_atom(Name) ->
  name_to_iolist(atom_to_list(Name), Tag);
name_to_iolist(Name, Tag) when is_binary(Name) ->
  name_to_iolist(binary_to_list(Name), Tag);
name_to_iolist(Name, Tag) when length(Name) =/= 0 ->
  true = lists:all(fun (C) -> xmerl_lib:is_char(C) end, Name),
  [$<, Tag, $>, xmerl_lib:export_text(Name), $<, $/, Tag, $>].

%% @spec params_to_iolist([ex_xmlrpc:value()]) -> iolist()
params_to_iolist(Params) ->
  [<<"<params>">>,
   [ value_to_iolist(Value) || Value <- Params ],
   <<"</params>">>].

%% @spec value_to_iolist(ex_xmlrpc:value()) -> iolist()
value_to_iolist(Value) ->
  [<<"<value>">>, term_to_iodata(Value), <<"</value">>].

%% @spec term_to_iodata(Term::ex_xmlrpc:value()) -> iodata()
term_to_iodata(I) when is_integer(I), I >= -2147483648, I =< 2147483647 ->
  [<<"<i4>">>, integer_to_list(I), <<"</i4">>];
term_to_iodata(Bool) when is_boolean(Bool) ->
  [<<"<boolean>">>, atom_to_list(Bool), <<"</boolean>">>];
term_to_iodata(Float) when is_float(Float) ->
  [<<"<double>">>, float_to_iolist(Float), <<"</double>">>];
term_to_iodata(Bin) when is_binary(Bin) ->
  binary_to_iodata(Bin);
term_to_iodata(Datetime) when is_tuple(Datetime) ->
  [<<"<dateTime.iso8601>">>,
   ex_iso8601:datetime_to_list(Datetime),
   <<"</dateTime.iso8601>">>];
term_to_iodata(List) when is_list(List) ->
  list_to_iodata(List).

%% @spec float_to_iolist(float()) -> iolist()
float_to_iolist(Float) ->
  List = float_to_list(Float),
  [IntToken, DecToken, ExpToken] = string:tokens(List, ".e"),
  {ok, [Sign, ExpAbs], []} = io_lib:fread("~-~u", ExpToken),
  case Sign of
    -1 -> ["0.", string:right(IntToken, ExpAbs, $0), DecToken];
    1 ->
      {IntCont, NewDec} = lists:split(ExpAbs, DecToken),
      [IntToken, IntCont, $., NewDec] end.

%% @spec binary_to_iodata(binary()) -> iodata()
binary_to_iodata(<<>>) ->
  <<"<string/>">>;
binary_to_iodata(Bin) ->
  case ex_binary:all(fun (C) -> xmerl_lib:is_char(C) end, Bin) of
    true -> [<<"<string>">>, xmerl_lib:export_text(Bin), <<"</string>">>];
    false -> [<<"<base64>">>, base64:encode(Bin), <<"</base64>">>] end.

%% @spec list_to_iodata(ex_xmlrpc:array() | ex_xmlrpc:struct()) -> iodata()
list_to_iodata([{}]) ->
  <<"<struct/>">>;
list_to_iodata(Struct = [{_Name, _Value} | _Tail]) ->
  [<<"<struct>">>,
   [ member_to_iolist(Member) || Member <- Struct ],
   <<"</struct>">>];
list_to_iodata([]) ->
  <<"<array><data/></array>">>;
list_to_iodata(Array) ->
  [<<"<array><data>">>,
   [ value_to_iolist(Value) || Value <- Array ],
   <<"</data></array>">>].

%% @spec member_to_iolist({ex_xmlrpc:name(), ex_xmlrpc:value()}) -> iolist()
member_to_iolist({Name, Value}) ->
  [<<"<member>">>,
   name_to_iolist(Name, <<"name">>),
   value_to_iolist(Value),
   <<"</member>">>].

%% @spec element_to_value(Element::#xmlElement{}) -> ex_xmlrpc:value()
element_to_value(#xmlElement{content = [#xmlText{value = Value}]}) ->
  list_to_binary(Value);
element_to_value(Element) ->
  typed_element_to_value(get_child_element(Element)).

%% @spec typed_element_to_value(Element::#xmlElement{}) -> ex_xmlrpc:value()
typed_element_to_value(#xmlElement{name = string, content = []}) ->
  <<>>;
typed_element_to_value(#xmlElement{name = Name,
                                   content = [#xmlText{value = Text}]}) ->
  text_to_value(Name, Text);
typed_element_to_value(Element = #xmlElement{name = array}) ->
  #xmlElement{content = Content} = get_child_element(Element),
  [ element_to_value(Child) || Child = #xmlElement{} <- Content ];
typed_element_to_value(#xmlElement{name = struct, content = Content}) ->
  Members = [ Member || Member = #xmlElement{} <- Content],
  case Members of
    [] -> [{}];
    _ -> lists:map(fun member_to_pair/1, Members) end.

%% @spec text_to_value(Type::text_type(), string()) -> ex_xmlrpc:value()
%%       where text_type() = i4 | int | boolean | double | string | base64
text_to_value(Type, Text) when Type =:= int; Type =:= i4 ->
  list_to_integer(Text);
text_to_value(boolean, Text) ->
  case Text of
    "1" -> true;
    "0" -> false end;
text_to_value(string, Text) ->
  list_to_binary(Text);
text_to_value(double, Text) ->
  list_to_float(Text);
text_to_value(dateTime.iso8601, Text) ->
  ex_iso8601:list_to_datetime(Text);
text_to_value(base64, Text) ->
  base64:decode(Text).

%% @spec member_to_pair(Member::#xmlElement{}) -> {binary(), ex_xmlrpc:value()}
member_to_pair(#xmlElement{content = Content}) ->
  member_to_pair2(Content).
%% @hidden
member_to_pair2([#xmlText{} | Content]) ->
  member_to_pair3(Content);
member_to_pair2(Content) ->
  member_to_pair3(Content).
%% @hidden
member_to_pair3([#xmlElement{content = [#xmlText{value = Name}]} | Content]) ->
  member_to_pair3(Content, list_to_binary(Name)).
%% @hidden
member_to_pair3([#xmlText{} | Content], Name) ->
  member_to_pair4(Content, Name);
member_to_pair3(Content, Name) ->
  member_to_pair4(Content, Name).
%% @hidden
member_to_pair4([Element | _Content], Name) ->
  {Name, element_to_value(Element)}.
