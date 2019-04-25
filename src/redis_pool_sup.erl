%%%-------------------------------------------------------------------
%% @doc redis_pool top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(redis_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([start_service/1,
				 stop_service/1]).
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_service(ServiceName)	->
		Spec = service_spec(ServiceName),
		supervisor:start_child(?MODULE, Spec).

stop_service(ServiceName)	->
		supervisor:terminate_child(?MODULE, ServiceName),
		supervisor:delete_child(?MODULE, ServiceName).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_one, 100, 100}, service_specs()}}.

service_specs()	->
		{ok, ServiceNames} = application:get_env(redis_pool, services),
		[service_spec(ServiceName) || ServiceName <- ServiceNames].

service_spec(ServiceName)	->
		{ok, Opts} = application:get_env(redis_pool, ServiceName),
		Host = proplists:get_value(host, Opts, "127.0.0.1"),
		Port = proplists:get_value(port, Opts, 6379),
		DB = proplists:get_value(db, Opts, 0),
		Password = proplists:get_value(password, Opts, ""),
		PoolSizeOrig = proplists:get_value(pool_size, Opts, 1),
		RedisPoolOpts = [ServiceName, PoolSizeOrig,
										 [eredis, eredis_client, eredis_parse],
										 {eredis, start_link, [Host, Port, DB, Password]}],
		{ServiceName, 
		 {cuesport, start_link, RedisPoolOpts},
		 permanent,
		 infinity,
		 supervisor,
		 [eredis]
		}.
											

%%====================================================================
%% Internal functions
%%====================================================================
