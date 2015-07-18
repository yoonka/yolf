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

%% timeout to wait for gen_serv implementations when shutting down the app
-define(SHUTDOWN_TIMEOUT, 5000).

%% Helper macros for declaring children of a supervisor
-define(SUPERVISOR(I), {I, {I, start_link, []}, permanent, infinity, supervisor, [I]}).
-define(WORKER(I), {I, {I, start_link, []}, permanent, ?SHUTDOWN_TIMEOUT, worker, [I]}).

-define(LOG_SUPERVISOR(S), lager:debug(<<"=== Starting supervisor:'~p'... ===">>, [S])).
-define(LOG_WORKER(S), lager:debug(<<"=== Starting worker:'~p'... ===">>, [S])).

-define(LOG_SUPERVISOR_INIT(S), lager:info(<<"=== Supervisor:'~p' started ===">>, [S])).
-define(LOG_WORKER_INIT(S), lager:info(<<"=== Worker:'~p' started ===">>, [S])).
