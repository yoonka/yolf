%% Copyright (c) 2016, Grzegorz Junka
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

-module(yocmd).

-export([chmod/2, mk_link/2, mk_dir/1]).

chmod(Name, Mode) ->
    yolog:i(<<"Change mode of '">>, Name, <<"' to ">>,
            {f, ".8B", Mode}, <<": ">>),
    check_file_op(file:change_mode(Name, Mode)).

mk_link(To, From) ->
    yolog:i(<<"Create link to '">>, To, <<"' from '">>, From, <<"': ">>),
    check_file_op(file:make_symlink(To, From)).

mk_dir(Name) ->
    yolog:i(<<"Create folder '">>, Name, <<"': ">>),
    check_file_op(file:make_dir(Name)).


check_file_op(ok) ->
    yolog:in(<<"Done.">>);
check_file_op({error, Err}) ->
    yolog:en(<<"Error:">>, endl, {f,"1000p",Err}),
    throw(Err).
