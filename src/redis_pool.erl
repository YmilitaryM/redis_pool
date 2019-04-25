%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%      Copyright (C) 2019 ... All rights reserved.
%%      FileName ：redis_pool.erl
%%      Create   ：Jin <ymilitarym@163.com
%%      Date     : 2019-04-25
%%      Describle: 
%%      
%%      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(redis_pool).

-export([q/2, qp/2, check/0, check/1]).
-include("logger.hrl").

q(Service, Command) ->
    try
        Pid = cuesport:get_worker(Service),
        case eredis:q(Pid, Command, get_timeout()) of
            {error, Reason} ->
                ?ERROR_MSG("redis q error: Service=~p, Command=~p, Error=~p",
                           [Service, Command, Reason]),
                {error, Reason};
            {ok, Value} ->
                {ok, Value}
        end
    catch
        exit:{timeout, _} ->
            ?ERROR_MSG("redis error: Service=~p, Command=~p, Error=~p",
                       [Service, Command, timeout]),
            {error, timeout};
				Class:Exception:StackTrace ->
            ?ERROR_MSG("redis error: Service=~p, Command=~p, Error=~p",
                       [Service, Command, {Class, {Exception, StackTrace}}]),
            {error, {Class, Exception}}
    end.

qp(Service, Commands) ->
    try
        Pid = cuesport:get_worker(Service),
        case eredis:qp(Pid, Commands, get_timeout()) of
            {error, Reason} ->
                ?ERROR_MSG("redis error: Service=~p, Commands=~p, Error=~p",
                           [Service, Commands, Reason]),
                {error, Reason};
            Values when is_list(Values)  ->
                check_qp_values(Service, Commands, Values),
                Values
        end
    catch
        exit:{timeout, _} ->
            ?ERROR_MSG("redis error: Service=~p, Commands=~p, Error=~p",
                       [Service, Commands, timeout]),
            {error, timeout};
				Class:Exception:StackTrace ->
            ?ERROR_MSG("redis error: Service=~p, Commands=~p, Error=~p",
                       [Service, Commands, {Class, {Exception, StackTrace}}]),
            {error, {Class, Exception}}
    end.

get_timeout() ->
    application:get_env(redis_pool, default_timeout, 5000).

check_qp_values(Service, Commands, Values) ->
    lists:foreach(
      fun({Command, {error, Reason}}) ->
              ?ERROR_MSG("redis error: Service=~p, Command=~p, Error=~p",
                         [Service, Command, Reason]),
              {error, Reason};
         ({_Command, {ok, Value}}) ->
              {ok, Value}
      end, lists:zip(Commands, Values)).

check() ->
    Clients = application:get_env(redis_pool, services, []),
    maps:from_list(lists:map(
                     fun(Client) ->
                             check(Client)
                     end, Clients)).

check(Client) ->
    try
        Pids = [Pid||{_, Pid}<-ets:tab2list(Client),
                     is_pid(Pid)],
        ResList = lists:map(
                    fun(Pid) ->
                            try eredis:q(Pid, [get, check_redis]) of
                                {ok, _} ->
                                    {Pid, ok};
                                Other ->
                                    {Pid, Other}
                            catch
                                C:E ->
                                    {Pid, {C,E}}
                            end
                    end, Pids),
        {Client, maps:from_list(ResList)}
    catch
        C:E ->
            {Client, {C,E}}
    end.
