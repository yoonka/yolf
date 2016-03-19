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

-module(yexec).


-export([is_cmd/1, sh_cmd/1, cmd/1]).

is_cmd(Bin) ->
    case sh_cmd(<< <<"which ">>/binary, Bin/binary >>) of
        {0, _} -> true;
        _ -> false
    end.

sh_cmd(Bin) ->
    EscBin = binary:replace(Bin, <<"\"">>, <<"\\\"">>, [global]),
    Cmd = << <<"sh -c \"">>/binary, EscBin/binary, <<" 2>&1\"">>/binary >>,
    cmd(Cmd).

cmd(Command) ->
    Args = [binary, in, eof, hide, exit_status, {line, 2048}],
    Port = open_port({spawn, Command}, Args),
    get_data(Port, {<<>>, []}).

get_data(Port, {Line, Lines}) ->
    receive
        {Port, {data, {eol, Bytes}}} ->
            NewLine = <<Line/binary, Bytes/binary>>,
            get_data(Port, {<<>>, [NewLine | Lines]});
        {Port, {data, {noeol, Bytes}}} ->
            get_data(Port, {<<Line/binary, Bytes/binary>>, Lines});
        {Port, eof} ->
            {exit_code(Port), lists:reverse(Lines)}
    end.

exit_code(Port) ->
    Port ! {self(), close},
    receive {Port, closed} -> true end,
    %% Remove EXIT message
    receive {'EXIT',  Port,  _} -> ok after 1 -> ok end,
    receive {Port, {exit_status, Code}} -> Code end.
