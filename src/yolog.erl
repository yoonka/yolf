%% Copyright (c) 2015-2016, Grzegorz Junka
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

%% o - owner log (use owner process' PID as log name)
-module(yolog).

-export(
   [init/1, stop/0,
    ti/1, ti/2, ti/3, ti/4, ti/5, ti/6, ti/7, ti/8,
    tin/1, tin/2, tin/3, tin/4, tin/5, tin/6, tin/7, tin/8,
    i/1, i/2, i/3, i/4, i/5, i/6, i/7, i/8,
    in/1, in/2, in/3, in/4, in/5, in/6, in/7, in/8]).

%%------------------------------------------------------------------------------

init(LogFileName) -> ylog:init(self(), yolf:to_list(LogFileName)).

stop() -> ylog:stop(self()).


i(List) when is_list(List) -> ylog:info(self(), List);
i(V1) -> i([V1]).

in(List) when is_list(List) -> ylog:infon(self(), List);
in(V1) -> in([V1]).

ti(List) when is_list(List) -> ylog:tinfo(self(), List);
ti(V1) -> ti([V1]).

tin(List) when is_list(List) -> ylog:tinfon(self(), List);
tin(V1) -> tin([V1]).

%%------------------------------------------------------------------------------

i(V1, V2) -> i([V1, V2]).
i(V1, V2, V3) -> i([V1, V2, V3]).
i(V1, V2, V3, V4) -> i([V1, V2, V3, V4]).
i(V1, V2, V3, V4, V5) -> i([V1, V2, V3, V4, V5]).
i(V1, V2, V3, V4, V5, V6) -> i([V1, V2, V3, V4, V5, V6]).
i(V1, V2, V3, V4, V5, V6, V7) -> i([V1, V2, V3, V4, V5, V6, V7]).
i(V1, V2, V3, V4, V5, V6, V7, V8) -> i([V1, V2, V3, V4, V5, V6, V7, V8]).

in(V1, V2) -> in([V1, V2]).
in(V1, V2, V3) -> in([V1, V2, V3]).
in(V1, V2, V3, V4) -> in([V1, V2, V3, V4]).
in(V1, V2, V3, V4, V5) -> in([V1, V2, V3, V4, V5]).
in(V1, V2, V3, V4, V5, V6) -> in([V1, V2, V3, V4, V5, V6]).
in(V1, V2, V3, V4, V5, V6, V7) -> in([V1, V2, V3, V4, V5, V6, V7]).
in(V1, V2, V3, V4, V5, V6, V7, V8) -> in([V1, V2, V3, V4, V5, V6, V7, V8]).

ti(V1, V2) -> ti([V1, V2]).
ti(V1, V2, V3) -> ti([V1, V2, V3]).
ti(V1, V2, V3, V4) -> ti([V1, V2, V3, V4]).
ti(V1, V2, V3, V4, V5) -> ti([V1, V2, V3, V4, V5]).
ti(V1, V2, V3, V4, V5, V6) -> ti([V1, V2, V3, V4, V5, V6]).
ti(V1, V2, V3, V4, V5, V6, V7) -> ti([V1, V2, V3, V4, V5, V6, V7]).
ti(V1, V2, V3, V4, V5, V6, V7, V8) -> ti([V1, V2, V3, V4, V5, V6, V7, V8]).

tin(V1, V2) -> tin([V1, V2]).
tin(V1, V2, V3) -> tin([V1, V2, V3]).
tin(V1, V2, V3, V4) -> tin([V1, V2, V3, V4]).
tin(V1, V2, V3, V4, V5) -> tin([V1, V2, V3, V4, V5]).
tin(V1, V2, V3, V4, V5, V6) -> tin([V1, V2, V3, V4, V5, V6]).
tin(V1, V2, V3, V4, V5, V6, V7) -> tin([V1, V2, V3, V4, V5, V6, V7]).
tin(V1, V2, V3, V4, V5, V6, V7, V8) -> tin([V1, V2, V3, V4, V5, V6, V7, V8]).
