%% Copyright (c) 2015, Grzegorz Junka
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% Redistributions of source code must retain the above copyright notice,
%% this list of conditions and the following disclaimer.
%% Redistributions in binary form must reproduce the above copyright notice,
%% this list of conditions and the following disclaimer in the documentation
%% and/or other materials provided with the distribution.
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
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
%% EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(yolf_log).

%% API
-export(
   [init/0, init/1, stop/0,
    tinfo/1, tinfo/2, tinfo/3, tinfo/4, tinfo/5, tinfo/6,
    tinfon/1, tinfon/2, tinfon/3, tinfon/4, tinfon/5, tinfon/6,
    info/1, info/2, info/3, info/4, info/5, info/6,
    infon/1, infon/2, infon/3, infon/4, infon/5, infon/6]).

%% Api
init() ->
    init_log(application:get_env(yolf, log_file_name)).

init_log(undefined) -> undefined;
init_log({ok, LogFileName}) -> init(LogFileName).

init(LogFileName) ->
    filelib:ensure_dir(LogFileName),
    case disk_log:open([{name, ?MODULE},
                        {file, LogFileName},
                        {format, external}]) of
        {ok, ?MODULE} ->
            ok;
        {repaired, ?MODULE, {recovered, Rec}, {badbytes, Bad}} ->
            io:format(<<"!! Repaired ~s; Recovered:~B, Bad Bytes:~B~n">>, [?MODULE, Rec, Bad])
    end.

stop() ->
    stop(disk_log:info(?MODULE)).

stop({error, no_such_log}) ->
    undefined;
stop(_List) ->
    disk_log:close(?MODULE).

info(List) when is_list(List) ->
    disk_log:blog(?MODULE, flatten(List, false));
info(V1) -> info([V1]).

infon(List) when is_list(List) ->
    disk_log:blog(?MODULE, flatten(List, true));
infon(V1) -> infon([V1]).

tinfo(List) when is_list(List) ->
    {T1, T2, T3} = now(),
    info([T1, <<":">>, T2, <<":">>, T3, <<"> ">>|List]);
tinfo(V1) -> tinfo([V1]).

tinfon(List) when is_list(List) ->
    {T1, T2, T3} = now(),
    infon([T1, <<":">>, T2, <<":">>, T3, <<"> ">>|List]);
tinfon(V1) -> tinfon([V1]).

info(V1, V2) -> info([V1, V2]).
info(V1, V2, V3) -> info([V1, V2, V3]).
info(V1, V2, V3, V4) -> info([V1, V2, V3, V4]).
info(V1, V2, V3, V4, V5) -> info([V1, V2, V3, V4, V5]).
info(V1, V2, V3, V4, V5, V6) -> info([V1, V2, V3, V4, V5, V6]).

infon(V1, V2) -> infon([V1, V2]).
infon(V1, V2, V3) -> infon([V1, V2, V3]).
infon(V1, V2, V3, V4) -> infon([V1, V2, V3, V4]).
infon(V1, V2, V3, V4, V5) -> infon([V1, V2, V3, V4, V5]).
infon(V1, V2, V3, V4, V5, V6) -> infon([V1, V2, V3, V4, V5, V6]).

tinfo(V1, V2) -> tinfo([V1, V2]).
tinfo(V1, V2, V3) -> tinfo([V1, V2, V3]).
tinfo(V1, V2, V3, V4) -> tinfo([V1, V2, V3, V4]).
tinfo(V1, V2, V3, V4, V5) -> tinfo([V1, V2, V3, V4, V5]).
tinfo(V1, V2, V3, V4, V5, V6) -> tinfo([V1, V2, V3, V4, V5, V6]).

tinfon(V1, V2) -> tinfon([V1, V2]).
tinfon(V1, V2, V3) -> tinfon([V1, V2, V3]).
tinfon(V1, V2, V3, V4) -> tinfon([V1, V2, V3, V4]).
tinfon(V1, V2, V3, V4, V5) -> tinfon([V1, V2, V3, V4, V5]).
tinfon(V1, V2, V3, V4, V5, V6) -> tinfon([V1, V2, V3, V4, V5, V6]).

%%% Internal functions
flatten(List, EndLine) ->
    lists:reverse(flatten(List, [], EndLine)).

flatten([H|T], Acc, EndLine) ->
    if
        is_binary(H) ->
            flatten(T, [H|Acc], EndLine);
        is_atom(H) ->
            flatten(T, [atom_to_binary(H, utf8)|Acc], EndLine);
        true ->
            flatten(T, [io_lib:print(H)|Acc], EndLine)
    end;
flatten([], Acc, true) ->
    [<<"\n">>|Acc];
flatten([], Acc, false) ->
    Acc;
flatten(Term, [], true) ->
    [<<"\n">>, Term];
flatten(Term, [], false) ->
    [Term].
