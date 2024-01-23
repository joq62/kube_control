%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%% Data: Nodename,cookie, node, status(allocated,free,stopped)
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(orchestrator).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").
-include("node.hrl").
-include("appl.hrl").

-include("control_config.hrl").
-include("infra_appls.hrl").


%% API
-export([
	 start_orchestrate/1,
	 create_workers/0,
	 create_worker/2,
	 delete_worker/1
	]).

%% admin




-export([
	 kill/0,
	 ping/0,
	 stop/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
		     
-record(state, {
		wanted_state,
		deployments
		
	       }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Allocate the node with least num of running applications
%% 
%% 
%% @end
%%--------------------------------------------------------------------
-spec  start_orchestrate(DeploymentSpec :: string()) -> ok | 
	  {error, Error :: term()}.
start_orchestrate(DeploymentSpec) ->
    gen_server:call(?SERVER,{start_orchestrate,DeploymentSpec},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Create a worker with a node name = NodeName and node dir=NodeDir. It's implict tha
%% the worker starts at current host and has the same cookie.
%% If the worker exists it will be killed and the dir will be deleted
%%  
%% @end
%%--------------------------------------------------------------------
-spec  create_workers() -> {ok,ListOfWorkers :: term()} | 
	  {error, Error :: term()}.
create_workers() ->
    gen_server:call(?SERVER,{create_workers},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Create a worker with a node name = NodeName and node dir=NodeDir. It's implict tha
%% the worker starts at current host and has the same cookie.
%% If the worker exists it will be killed and the dir will be deleted
%%  
%% @end
%%--------------------------------------------------------------------
-spec  create_worker(NodeName :: string(), NodeDir :: string()) -> {ok,Node :: node()} | 
	  {error, Error :: term()}.
create_worker(NodeName, NodeDir) ->
    gen_server:call(?SERVER,{create_worker,NodeName, NodeDir},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Worker vm is stopped and worker dir is deleted
%%  
%% @end
%%--------------------------------------------------------------------
-spec  delete_worker(NodeName :: string()) -> {ok,Node :: node()} | 
	  {error, Error :: term()}.
delete_worker(NodeName) ->
    gen_server:call(?SERVER,{delete_worker,NodeName},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
kill()->
    gen_server:call(?SERVER, {kill},infinity).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec worker_list() -> DeploymentList :: term() | Error::term().
worker_list()-> 
    gen_server:call(?SERVER, {worker_list},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec ping() -> pong | Error::term().
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).


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


%stop()-> gen_server:cast(?SERVER, {stop}).
stop()-> gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

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
    {ok, #state{
	    wanted_state=[],
	    deployments=[]
	    
	   }}.

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


handle_call({start_orchestrate,DeploymentSpec}, _From, State) ->
    Reply = case lib_orchestrator:create_workers() of
		{error,Reason}->
		    NewState=State,
		    {error,Reason};
		{ok,RunningWorkers}->
		    case lib_orchestrator:load_start_infra(?InfraAppls,RunningWorkers) of
			{error,Reason}->
			    NewState=State,
			    {error,Reason};
			{ok,InfraDeployments}->
			    case lib_orchestrator:wanted_state(DeploymentSpec) of
				{error,Reason}->
				    NewDeployments=lists:append(InfraDeployments,State#state.deployments),
				    NewState=State#state{deployments=NewDeployments},	    
				    {error,Reason};
				{ok,LocalWantedAppls}->
				    case lib_orchestrator:load_start_wanted_state(LocalWantedAppls) of
					{error,Reason}->
					    NewDeployments=lists:append(InfraDeployments,State#state.deployments),
					    NewState=State#state{deployments=NewDeployments,
								 wanted_state=LocalWantedAppls},	    
					    {error,Reason};
					LoadStartResult->
					    
					    ?LOG_NOTICE("LoadStartResult ",[LoadStartResult]),
					    
					    OkStart=[DeploymentInfo||{ok,DeploymentInfo}<-LoadStartResult],
					    NewDeployments=lists:append([OkStart,InfraDeployments,State#state.deployments]),
					    NewState=State#state{deployments=NewDeployments,
								 wanted_state=LocalWantedAppls},	    
					    {ok,LoadStartResult}
				    end
			    end
		    end
	    end,
    {reply, Reply, NewState};

handle_call({ping}, _From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(UnMatchedSignal, From, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal, From,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,UnMatchedSignal, From]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({stop}, State) ->
    
    {stop,normal,ok,State};

handle_cast(UnMatchedSignal, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal,?MODULE,?LINE}]),
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

handle_info({nodedown,Node}, State) ->
    io:format("nodedown,Node  ~p~n",[{Node,?MODULE,?LINE}]),
    NewState=State,
    {noreply, NewState};


handle_info(Info, State) ->
    io:format("unmatched_signal ~p~n",[{Info,?MODULE,?LINE}]),
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
