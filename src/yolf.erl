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

-module(yolf).

-export([to_list/1,
         to_atom/2,
         to_existing_atom/2,
         to_integer/1,
         to_boolean/1,
         to_binaries/1,
         to_binary/1,
         to_ip/1,
         to_seconds/1]).

-export([bin_timestamp/0]).

-export([padding_2/1, last_2/1, padding/2, last/2, bin_fill/2]).

-export([trim/1]).

-export([is_string/1]).


to_list(List) when is_list(List)  -> List;
to_list(Bin)  when is_binary(Bin) -> binary_to_list(Bin).

to_atom(Atom, Suffix) when is_binary(Suffix) ->
    binary_to_atom(<< (to_binary(Atom))/binary, Suffix/binary >>, utf8).

to_existing_atom(Prefix, Atom) when is_binary(Prefix) ->
    binary_to_existing_atom(<< Prefix/binary, (to_binary(Atom))/binary >>, utf8).

to_integer(Int)  when is_integer(Int) -> Int;
to_integer(List) when is_list(List)   -> list_to_integer(List);
to_integer(Int)  when is_binary(Int)  -> list_to_integer(binary_to_list(Int)).

to_boolean(true)        -> true;
to_boolean(false)       -> false;
to_boolean(<<"true">>)  -> true;
to_boolean(<<"false">>) -> false;
to_boolean("true")      -> true;
to_boolean("false")     -> false;
to_boolean(_)           -> exit(badarg).

to_binaries(List) when is_list(List) -> [to_binary(X) || X <- List].


to_binary({IP1, IP2, IP3, IP4}) ->
    B1 = to_binary(IP1),
    B2 = to_binary(IP2),
    B3 = to_binary(IP3),
    B4 = to_binary(IP4),
    Dot = <<".">>,
    << B1/binary, Dot/binary, B2/binary, Dot/binary, B3/binary, Dot/binary, B4/binary >>;

to_binary({{Y, M, D}, {H, N, S}}) ->
    BY = to_binary(Y),
    BM = padding_2(to_binary(M)),
    BD = padding_2(to_binary(D)),
    BH = padding_2(to_binary(H)),
    BN = padding_2(to_binary(N)),
    BS = padding_2(to_binary(S)),
    Dash = <<"-">>,
    Colon = <<":">>,
    << BY/binary, Dash/binary, BM/binary, Dash/binary, BD/binary, <<" ">>/binary,
       BH/binary, Colon/binary, BN/binary, Colon/binary, BS/binary >>;

to_binary(Int) when is_integer(Int) ->
    list_to_binary(integer_to_list(Int));

to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);

to_binary(Binary) when is_binary(Binary) ->
    Binary;

to_binary(List) when is_list(List) ->
    list_to_binary(List).


to_ip(Ip) when is_binary(Ip) ->
    [B1, B2, B3, B4] = binary:split(Ip, <<".">>, [global]),
    IP1 = to_integer(B1),
    IP2 = to_integer(B2),
    IP3 = to_integer(B3),
    IP4 = to_integer(B4),
    {IP1, IP2, IP3, IP4}.


to_seconds({Mega, Sec, Micro}) ->
    Mega * 1000000 + Sec + if Micro > 500000 -> 1; true -> 0 end.


bin_timestamp() ->
    TS = {_, _, Micro} = os:timestamp(),
    {{Y, M, D}, {H, N, S}} = calendar:now_to_universal_time(TS),
    BY = to_binary(Y),
    BM = padding_2(to_binary(M)),
    BD = padding_2(to_binary(D)),
    BH = padding_2(to_binary(H)),
    BN = padding_2(to_binary(N)),
    BS = padding_2(to_binary(S)),
    BC = padding(6, to_binary(Micro)),
    Dash = <<"-">>,
    Colon = <<":">>,
    << BY/binary, Dash/binary, BM/binary, Dash/binary, BD/binary, <<" ">>/binary,
       BH/binary, Colon/binary, BN/binary, Colon/binary, BS/binary,
       <<".">>/binary, BC/binary >>.


padding_2(Binary) when size(Binary) =:= 2 -> Binary;
padding_2(Binary) -> last_2(<< <<"00">>/binary, Binary/binary >>).

last_2(<<_, T:2/binary>>) -> T;
last_2(<<T:2/binary>>) -> T.


padding(X, Binary) when size(Binary) =:= X -> Binary;
padding(X, Binary) -> last(X, <<(bin_fill($0, X))/binary, Binary/binary>>).

last(X, Bin) ->
    binary:part(Bin, {byte_size(Bin), -X}).


bin_fill(Int, Len) ->
    Fun = fun F(0, A) -> A; F(Z, A) -> F(Z-1, [Int|A]) end,
    list_to_binary(Fun(Len, [])).

%%------------------------------------------------------------------------------

%% Strip all leading and/or trailing white characters
trim(What) ->
    re:replace(What, "(^\\s+)|(\\s+$)", "", [global, {return, list}]).

is_string(List) -> is_list(List) andalso is_integer(hd(List)).
