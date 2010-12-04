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

%% @type datetime() = {{Year::integer(), Month::integer(), Day::integer()},
%%                     {Hour::integer(), Min::integer(), Sec::integer()}}.

%% @doc Convert datetime tuples from and to ISO 8601 format.
%%      This module does not handle microseconds and timezones.
-module(ex_iso8601).
-author('Anthony Ramine <nox@dev-extend.eu>').

-export([datetime_to_list/1,
         list_to_datetime/1]).

-define(is_year(Y), is_integer(Y) andalso Y >= 0 andalso Y =< 9999).

-define(is_time(H, M, S), is_integer(H) andalso H >= 0 andalso H =< 23 andalso
                          is_integer(M) andalso M >= 0 andalso M =< 59 andalso
                          is_integer(S) andalso S >= 0 andalso S =< 59).


%% @spec datetime_to_list(Datetime::datetime()) -> list()
%% @doc Convert a datetime tuple to its ISO 8601 representation.
datetime_to_list({{Y, M, D}, {H, Min, S}}) when ?is_year(Y),
                                                ?is_time(H, Min, S) ->
  true = calendar:valid_date(Y, M, D),
  io_lib:format("~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0B",
                [Y, M, D, H, Min, S]).

%% @spec list_to_datetime(string()) -> datetime()
%% @doc Convert an ISO 8601 date string to a datetime tuple.
list_to_datetime(List) ->
  {ok, [Y, M, D, H, Min, S], []} = io_lib:fread("~4u~2u~2uT~2u~2u~2u", List),
  true = calendar:valid_date(Y, M, D) andalso ?is_time(H, Min, S),
  {{Y, M, D}, {H, Min, S}}.
