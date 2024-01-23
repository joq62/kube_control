%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%% Control starts and handles applications log,rd, etcd and  system_boot
%%% node_ctrl,    
%%% appl_ctrl,orchestrator and control
%%% @end
%%% Created :  2 Jun 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(control).

-behaviour(gen_server).  
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("control.resource_discovery").
-include("control_config.hrl").
-include("log.api").

-define(InfraSpecId,"basic"). 
-include("node.hrl").
-include("appl.hrl").


-define(BuildPath,"ebin").

%% API

%% Application handling API

-export([
	 load_start/1,      
	 stop_unload/1,
	 reload/2,
	 get_deployments/0
	]).

%% Oam handling API

-export([
%	 all/0,
%	 all_nodes/0,
%	 all_providers/0,
%	 where_is/1,
%	 is_wanted_state/0
	]).



%% Debug API
-export([
%	 create_worker/1,
%	 delete_worker/1,
%	 load_provider/1,
%	 start/1,
%	 stop/1,
%	 unload/1
	
	 
	]).


-export([start/0,
	 ping/0]).


-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

%% Record and Data
-record(state, {
		infra_spec,
		deployment_spec,
		deployments}).

%% Table or Data models
%% ClusterSpec: Workers [{HostName,NumWorkers}],CookieStr,MainDir, 
%% ProviderSpec: appl_name,vsn,app,erl_args,git_path
%% DeploymentRecord: node_name,node,app, dir,provider,host

%% WorkerDeployment: DeploymentId, ProviderSpec, DeploymentRecord
%% Deployment: DeploymentId, ProviderId, Vsn, App, NodeName, HostName, NodeDir, ProviderDir,GitPath, Status : {status, time()}  
%% Static if in file : DeploymentSpecId, ProviderId, Vsn, App,ProviderDir,GitPath
%% Runtime:  DeploymentId,NodeName, HostName, NodeDir, Status
%% 


%%%===================================================================
%%% API
%%%===================================================================
%% DeploymentLog 
%% HealthLog

%%--------------------------------------------------------------------
%% @doc
%% create_deployment(ApplicationSpec)-> {ok,DeploymentId}|{error,Reason}
%% 1) Allocate a free worker node on the host
%% 2) Load needed infrastructure applications
%%    - log
%%    - resource discovery 
%% 3) start needed infrastructure applications
%%    - log
%%    - resource discovery
%% 4) Load wanted application
%%       - resource discovery 
%% 5) start wanted applications 
%%  
%% deploy(ApplicationSpec)-> {ok,DeploymentId}|{error,Reason}
%% Data (ApplicationSpec, worker node, running applications, DeploymentId,time_of_creation)
%% State deployed_applications 
%% DeploymentLog
%% @end
%%--------------------------------------------------------------------
-spec create_deployment(ApplicationSpec :: string()) -> {ok,DeploymentId :: integer()} | 
	  {error, Error :: term()}.
create_deployment(ApplicationSpec) ->
    gen_server:call(?SERVER,{create_deployment,ApplicationSpec},infinity).





%% delete_application(DeploymentId)-> ok | {error,Reason}
%% 1) stop the application
%% 2) unload the application
%% 3) delete application directory
%% Data (ApplicationSpec, worker node, running applications, DeploymentId,time_of_creation)
%% State deployed_applications 
%% DeploymentLog 

%% delete_node(Node) -> ok | {error,Reason}
%% 1) stop the node
%% 2) delete node dir
%% Data (ApplicationSpec, worker node, running applications, DeploymentId,time_of_creation)
%% State deployed_applications 
%% DeploymentLog 









%%--------------------------------------------------------------------
%% @doc
%% Loads and starts a provider on a free worker node.
%% 1) It allocates a free worker 
%% 2) Loads the 
%% @end
%%--------------------------------------------------------------------
-spec load_start(ProviderSpec :: string()) -> {ok,DeploymentId :: integer()} | 
	  {error, Error :: term()}.
%%  Tabels or State
%% Deployment: {DeploymentId,ProviderSpec,date(),time()}
%%  Deployments: [Deployment]

load_start(ProviderSpec) ->
    gen_server:call(?SERVER,{load_start,ProviderSpec},infinity).

%%--------------------------------------------------------------------
%% @doc
%% This function is an user interface to be complementary to automated
%% stop and unload a provider at this host.
%% In v1.0.0 the deployment will not be persistant   
%% @end
%%--------------------------------------------------------------------
-spec stop_unload(DeploymentId :: integer()) -> ok | 
	  {error, Error :: term()}.
%%  Tabels or State
%% Deployment: {DeploymentId,ProviderSpec,date(),time()}
%%  Deployments: [Deployment]

stop_unload(DeploymentId) ->
    gen_server:call(?SERVER,{stop_unload,DeploymentId},infinity).


%% xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


%--------------------------------------------------------------------
%% @doc
%% reload(DeploymentId) will stop_unload and load and start the provider   
%% @end
%%--------------------------------------------------------------------
-spec reload(DeploymentId :: integer(),ProviderSpec :: string()) -> {ok,NewDeploymentId :: integer()} | 
	  {error, Error :: term()}.
%%  Tabels or State
%% Deployment: {DeploymentId,ProviderSpec,date(),time()}
%%  Deployments: [Deployment]

reload(DeploymentId,ProviderSpec) ->
    gen_server:call(?SERVER,{reload,DeploymentId,ProviderSpec},infinity).

%--------------------------------------------------------------------
%% @doc
%% get_deployments returns list of deployed providers   
%% @end
%%--------------------------------------------------------------------
-spec get_deployments() -> DeploymentList :: term() | 
	  {error, Error :: term()}.
%%  Tabels or State
%% Deployment: {DeploymentId,ProviderSpec,date(),time()}
%%  Deployments: [Deployment]

get_deployments() ->
    gen_server:call(?SERVER,{get_deployments},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Create provider directory and starts the slave node 
%% @end
%%--------------------------------------------------------------------
-spec create_provider(Deployment :: string()) -> ok | 
	  {error, Error :: [already_started]} | 
	  {error, Error :: term()}.
%%  Tabels or State
%%  deployments: {Deployment,DeploymentTime,State(created,loaded, started, stopped, unloaded,deleted,error)

create_provider(Deployment) ->
    gen_server:call(?SERVER,{create_provider,Deployment},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Delete provider directory and stop the slave node
%% @end
%%--------------------------------------------------------------------
-spec delete_provider(Deployment :: string()) -> ok | 
	  {error, Error :: [already_started]} |
	  {error, Error :: term()}.
%%  Tabels or State
%%  deployments: {Deployment,DeploymentTime,State(created,loaded, started, stopped, unloaded,deleted,error)

delete_provider(Deployment) ->
    gen_server:call(?SERVER,{delete_provider,Deployment},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Load the provider application on to the created slave node 
%% @end
%%--------------------------------------------------------------------
-spec load_provider(Deployment :: string()) -> ok | 
	  {error, Error :: [already_started]} |
	  {error, Error :: [node_not_started]} |
	  {error, Error :: term()}.
%%  Tabels or State
%%  deployments: {Deployment,DeploymentTime,State(created,loaded, started, stopped, unloaded,deleted,error)

load_provider(Deployment) ->
    gen_server:call(?SERVER,{load_provider,Deployment},infinity).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec ping() -> pong | Error::term().
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.
init([]) ->

    ?LOG_NOTICE("Server started ",[?MODULE]),
    
    TimeOut=0,
 
    {ok, #state{
	    infra_spec=?InfraSpec,
	    deployments=[]},
     TimeOut}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.



handle_call({load_start,ProviderSpec}, _From, State) ->
    Reply = case control_provider_server:load_provider(ProviderSpec) of
		{error,Reason}->
		    NewState=State,
		    {error,Reason};
		{ok,DeploymentId}->
		    case control_provider_server:start_provider(DeploymentId) of
			{error,Reason}->
			    NewState=State,
			    {error,Reason};
			ok->
			    Deployments=State#state.deployments,
			    NewState=State#state{deployments=[{DeploymentId,ProviderSpec,date(),time()}|Deployments]},
			    {ok,DeploymentId}
		    end
	    end,
						 
    {reply, Reply, NewState};

handle_call({stop_unload,DeploymentId}, _From, State) ->
    Reply = case control_provider_server:stop_provider(DeploymentId) of
		{error,Reason}->
		    NewState=State,
		    {error,Reason};
		ok->
		    case control_provider_server:unload_provider(DeploymentId) of
			{error,Reason}->
			    NewState=State,
			    {error,Reason};
			ok->
			    ReducedDeployments=lists:keydelete(DeploymentId,1,State#state.deployments),
			    NewState=State#state{deployments=ReducedDeployments},
			    ok
		    end
	    end,
    {reply, Reply, NewState};

handle_call({reload,DeploymentId,ProviderSpec}, _From, State) ->
    Reply = case control_provider_server:stop_provider(DeploymentId) of
		{error,Reason}->
		    NewState=State,
		    {error,Reason};
		ok->
		    case control_provider_server:unload_provider(DeploymentId) of
			{error,Reason}->
			    NewState=State,
			    {error,Reason};
			ok->
			    case control_provider_server:load_provider(ProviderSpec) of
				{error,Reason}->
				    NewState=State,
				    {error,Reason};
				{ok,NewDeploymentId}->
				    case control_provider_server:start_provider(NewDeploymentId) of
					{error,Reason}->
					    NewState=State,
					    {error,Reason};
					ok->
					    ReducedDeployments=lists:keydelete(DeploymentId,1,State#state.deployments),
					    NewState=State#state{deployments=[{NewDeploymentId,ProviderSpec,date(),time()}|ReducedDeployments]},
					    ok
				    end
			    end
		    end
	    end,
    {reply, Reply, NewState}; 

handle_call({get_deployments}, _From, State) ->
    Reply = State#state.deployments,
    {reply, Reply, State};


handle_call({ping}, _From, State) ->
    Reply = pong,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info(timeout, State) ->
    
    %% 1. Connect nodes
    
    ThisNode=node(),
    {ok,ConnecNodes}=etcd_infra:get_connect_nodes(?InfraSpecId),
    ConnectR=[{net_adm:ping(N),N}||N<-ConnecNodes],
	
    %% 2. Announce to resource_discovery
    Interval=10*1000,
    Iterations=10,
    true=check_rd_running(Interval,Iterations,false),
    [rd:add_local_resource(ResourceType,Resource)||{ResourceType,Resource}<-?LocalResourceTuples],
    [rd:add_target_resource_type(TargetType)||TargetType<-?TargetTypes],
    rd:trade_resources(),
    ok=rd:detect_target_resources(?TargetTypes,?MaxDetectTime),
   
    %%---- start_orchestration

    StartOrchestratorResult=orchestrator:start_orchestrate(?DeploymentSpec),
    ?LOG_NOTICE("StartOrchestratorResult ",[StartOrchestratorResult]),
    
    %%---- 
    
	
    
    
    {noreply, State};

handle_info(Info, State) ->
    glurk=Info,
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_rd_running(_Interval,N_,true)->
    true;
check_rd_running(Interval,0,true)->
    true;
check_rd_running(Interval,0,false)->
    false;
check_rd_running(Interval,N,IsRunning)->
    case rpc:call(node(),rd,ping,[],5000) of
	pong->
	    NewIsRunning=true,
	    NewN=N;
	_->
	    timer:sleep(Interval),
	    NewIsRunning=false,
	    NewN=N-1
    end,
    check_rd_running(Interval,NewN,NewIsRunning).
	 
