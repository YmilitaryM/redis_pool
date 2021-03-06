%%%-------------------------------------------------------------------
%% @doc redis_pool public API
%% @end
%%%-------------------------------------------------------------------

-module(redis_pool_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
		Result = redis_pool_sup:start_link(),
    spawn(fun()->redis_pool:check() end),
    Result.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
