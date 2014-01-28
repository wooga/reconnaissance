-module(reconnaissance_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Port}           = application:get_env(reconnaissance, port),
    {ok, CallbackModule} = application:get_env(reconnaissance, callback_module),
    Child = {reconnaissance,
         {reconnaissance, start_link, [Port, CallbackModule]},
        permanent, 5000, worker, [reconnaissance]},
    {ok, { {one_for_one, 5, 10}, [Child]} }.

