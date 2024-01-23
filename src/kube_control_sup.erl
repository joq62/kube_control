%%%-------------------------------------------------------------------
%% @doc control top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(kube_control_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
		  #{id => system_boot,               
		    start => {system_boot,start_link,[]}} ,
		  #{id => kube_control,               
		    start => {kube_control,start_link,[]}}
		 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
