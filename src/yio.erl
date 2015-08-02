%% Copyright (c) 2015, Grzegorz Junka
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% * Redistributions of source code must retain the above copyright notice,
%%   this list of conditions and the following disclaimer.
%% * Redistributions in binary form must reproduce the above copyright notice,
%%   this list of conditions and the following disclaimer in the documentation
%%   and/or other materials provided with the distribution.
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

-module(yio).

-export(
   [info/1, info/2, info/3, info/4, info/5, info/6, info/7,
    infon/1, infon/2, infon/3, infon/4, infon/5, infon/6, infon/7,
    error/1, error/2, error/3, error/4, error/5, error/6, error/7,
    errorn/1, errorn/2, errorn/3, errorn/4, errorn/5, errorn/6, errorn/7]).

-compile({no_auto_import, [error/1]}).

info(List) when is_list(List) ->
    log(info, List, false);
info(V1) -> info([V1]).

infon(List) when is_list(List) ->
    log(info, List, true);
infon(V1) -> infon([V1]).

error(List) when is_list(List) ->
    log(error, List, false);
error(V1) -> error([V1]).

errorn(List) when is_list(List) ->
    log(error, List, true);
errorn(V1) -> errorn([V1]).

info(V1, V2) -> info([V1, V2]).
info(V1, V2, V3) -> info([V1, V2, V3]).
info(V1, V2, V3, V4) -> info([V1, V2, V3, V4]).
info(V1, V2, V3, V4, V5) -> info([V1, V2, V3, V4, V5]).
info(V1, V2, V3, V4, V5, V6) -> info([V1, V2, V3, V4, V5, V6]).
info(V1, V2, V3, V4, V5, V6, V7) -> info([V1, V2, V3, V4, V5, V6, V7]).

infon(V1, V2) -> info([V1, V2, endl]).
infon(V1, V2, V3) -> infon([V1, V2, V3]).
infon(V1, V2, V3, V4) -> infon([V1, V2, V3, V4]).
infon(V1, V2, V3, V4, V5) -> infon([V1, V2, V3, V4, V5]).
infon(V1, V2, V3, V4, V5, V6) -> infon([V1, V2, V3, V4, V5, V6]).
infon(V1, V2, V3, V4, V5, V6, V7) -> infon([V1, V2, V3, V4, V5, V6, V7]).

error(V1, V2) -> error([V1, V2]).
error(V1, V2, V3) -> error([V1, V2, V3]).
error(V1, V2, V3, V4) -> error([V1, V2, V3, V4]).
error(V1, V2, V3, V4, V5) -> error([V1, V2, V3, V4, V5]).
error(V1, V2, V3, V4, V5, V6) -> error([V1, V2, V3, V4, V5, V6]).
error(V1, V2, V3, V4, V5, V6, V7) -> error([V1, V2, V3, V4, V5, V6, V7]).

errorn(V1, V2) -> errorn([V1, V2]).
errorn(V1, V2, V3) -> errorn([V1, V2, V3]).
errorn(V1, V2, V3, V4) -> errorn([V1, V2, V3, V4]).
errorn(V1, V2, V3, V4, V5) -> errorn([V1, V2, V3, V4, V5]).
errorn(V1, V2, V3, V4, V5, V6) -> errorn([V1, V2, V3, V4, V5, V6]).
errorn(V1, V2, V3, V4, V5, V6, V7) -> errorn([V1, V2, V3, V4, V5, V6, V7]).

%%% Internal functions
log(info, Log, EndLine) ->
    out_log(standard_io, Log, EndLine);
log(error, Log, EndLine) ->
    out_log(standard_error, Log, EndLine).

out_log(IO, Log, EndLine) ->
    io:format(IO, yolf_log:flatten(Log, EndLine), []).
