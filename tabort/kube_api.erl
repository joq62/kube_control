%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%% Data: Nodename,cookie, node, status(allocated,free,stopped)
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(kube_api).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").
-include("node.hrl").

-include("control_config.hrl").
-include("infra_appls.hrl").


%% API
-export([
	 running_worker_nodes/0,
	 node_info_list/0,
	 
	 create_workers/0,
	 create_worker/2,
	 delete_worker/1,
	 allocate/0
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
		running_worker_nodes,
		node_info_list,
		num_workers,
		hostname,
		cookie_str

	       }).

%%%===================================================================
%%% API
%%%===================================================================



%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% @end
%%--------------------------------------------------------------------
-spec  running_worker_nodes() -> {NodeInfoList :: term()} | 
	  {error, Error :: term()}.
running_worker_nodes() ->
    gen_server:call(?SERVER,{running_worker_nodes},infinity).

%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% @end
%%--------------------------------------------------------------------
-spec  node_info_list() -> {NodeInfoList :: term()} | 
	  {error, Error :: term()}.
node_info_list() ->
    gen_server:call(?SERVER,{node_info_list},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Allocate the node with least num of running applications
%% 
%% 
%% @end
%%--------------------------------------------------------------------
-spec  allocate() -> {NodeInfo :: term()} | 
	  {error, Error :: term()}.
allocate() ->
    gen_server:call(?SERVER,{allocate},infinity).

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
  
    {ok,HostName}=net:gethostname(),
    {ok,NumWorkers}=etcd_infra:get_num_workers(?InfraSpec,HostName),
    {ok,CookieStr}=etcd_infra:get_cookie_str(?InfraSpec),
    NodeInfoList=lib_node_ctrl:create_node_info(NumWorkers,HostName,CookieStr),
    
  %  io:format("ListOfNodeInfo ~p~n",[{ListOfNodeInfo,?MODULE,?LINE}]),
    
     
    ?LOG_NOTICE("Server started ",[?MODULE]),
    {ok, #state{
	    running_worker_nodes=[],
	    node_info_list=NodeInfoList,
	    num_workers=NumWorkers,
	    hostname=HostName,
	    cookie_str=CookieStr
	    
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




handle_call({create_worker,NodeName,WorkerDir}, _From, State) ->
    WorkerNode=list_to_atom(NodeName++"@"++State#state.hostname),    
    NodeInfo=#node_info{worker_node=WorkerNode,
			      worker_dir=WorkerDir,
			      nodename=NodeName,
			      hostname=State#state.hostname,
			      cookie_str=State#state.cookie_str},
    
    Reply=case lib_node_ctrl:create_worker(NodeInfo) of
	      {error,Reason}->
		  NewState=State,
		  {error,Reason};
	      {ok,NewNodeInfo}->
		  erlang:monitor_node(NewNodeInfo#node_info.worker_node,true),
		  NewRunningWorkerNodes=[NewNodeInfo|State#state.running_worker_nodes],
		  NewState=State#state{running_worker_nodes=NewRunningWorkerNodes},
		  {ok,NodeInfo}
	  end,
    {reply, Reply, NewState};


handle_call({delete_worker,NodeInfo}, _From, State) ->
    Reply=case lists:member(NodeInfo,State#state.running_worker_nodes) of
	      false->
		  NewState=State,
		  {error,["NodeInfo doesnt exists inrunning_worker_nodes",NodeInfo,State#state.running_worker_nodes,?MODULE,?LINE]};
	      true->
		  erlang:monitor_node(NodeInfo#node_info.worker_node,false),
		  lib_node_ctrl:delete_worker(NodeInfo),
		  NewRunningWorkerNodes=lists:delete(NodeInfo,State#state.running_worker_nodes),
		  NewState=State#state{running_worker_nodes=NewRunningWorkerNodes},
		  ok
	  end,

    {reply, Reply, NewState};


handle_call({allocate}, _From, State) ->
    Reply=case lib_node_ctrl:allocate(State#state.running_worker_nodes) of
	      {error,Reason}->
		  NewState=State,
		  {error,Reason};
	      {ok,NodeInfo,NewRunningWorkerNodes}->
		  NewState=State#state{running_worker_nodes=NewRunningWorkerNodes},
		  {ok,NodeInfo}
	  end,
    
    {reply, Reply, NewState};

handle_call({node_info_list}, _From, State) ->
    Reply=State#state.node_info_list,
    {reply, Reply, State};

handle_call({running_worker_nodes}, _From, State) ->
    Reply=State#state.running_worker_nodes,
    {reply, Reply, State};

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
    erlang:monitor_node(Node,false),
    case node_info:find(State#state.running_worker_nodes,Node) of
	false->
	    NewState=State,
	    {error,["Node doesnt exist i running_worker_nodes ",Node,?MODULE,?LINE]};
	NodeInfo->
	    case lib_node_ctrl:create_worker(NodeInfo) of
		{error,Reason}->
		    NewState=State,
		    {error,Reason};
		{ok,NewNodeInfo}->
		    io:format("NewNodeInfo  ~p~n",[{NewNodeInfo,?MODULE,?LINE}]),
		    erlang:monitor_node(NewNodeInfo#node_info.worker_node,true),
		    RunningDeleted=lists:delete(NodeInfo,State#state.running_worker_nodes),
		    NewRunningList=[NewNodeInfo|RunningDeleted],
		    NewState=State#state{running_worker_nodes=NewRunningList}
			
	    end
    end,
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
