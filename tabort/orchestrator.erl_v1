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
-include("node_record.hrl").

-define(InfraSpecId,"basic"). 

%% API


%% admin
-export([
	 get_wanted_state/0,
	 is_wanted_state/0,
	 
	 create_start_node/2,
	 stop_delete_node/1,
	 allocate/0,
	 free/1,
	 
	
	 is_alive/1,
	 get_free/0,
	 get_allocated/0,
	 get_deleted/0,
	 get_not_created/0,
	 
	 dbg_get_state/0,

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
		wanted_state,   %% List of {Nodename,NodeDir} 
		wanted_state_status,
		node_records    %% 
	       }).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
kill()->
    gen_server:call(?SERVER, {kill},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec get_wanted_state() -> WantedState :: term().

get_wanted_state()->
    gen_server:call(?SERVER, {get_wanted_state},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec is_wanted_state() -> true | {false,WhyNot :: term()}.

is_wanted_state()->
    gen_server:call(?SERVER, {is_wanted_state},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec allocate() -> {ok,Id :: integer(), Node :: node(),NodeDir :: string()} | {error, Reason :: term()}.

allocate()->
    gen_server:call(?SERVER, {allocate},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec free(Id :: integer()) -> ok | {error, Reason :: term()}.

free(Id)->
    gen_server:call(?SERVER, {free,Id},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec get_free() -> ListOfFreeNodeRecords :: term().

get_free()->
    gen_server:call(?SERVER, {get_free},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec get_allocated() -> ListOfAllocatedNodeRecords :: term().

get_allocated()->
    gen_server:call(?SERVER, {get_allocated},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec get_deleted() -> ListOfDeletedNodeRecords :: term().

get_deleted()->
    gen_server:call(?SERVER, {get_deleted},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec get_not_created() -> ListOfNotCreatedNodeRecords :: term().

get_not_created()->
    gen_server:call(?SERVER, {get_not_created},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec create_start_node(NodeName :: string(),NodeDir :: string()) -> {ok,ProviderNode :: node(),NodeDir :: string()} | {error, Error :: term()}.

create_start_node(NodeName,NodeDir)->
    gen_server:call(?SERVER, {create_start_node,NodeName,NodeDir},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec stop_delete_node(ProviderNode :: atom()) -> ok | {error, Error :: term()}.

stop_delete_node(ProviderNode)->
    gen_server:call(?SERVER, {stop_delete_node,ProviderNode},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec is_alive(DeploymentRecord :: term()) -> IsDeployed :: boolean() | {error, Error :: term()}.

is_alive(ProviderNode)->
    gen_server:call(?SERVER, {is_alive,ProviderNode},infinity).


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
%% 
%% @end
%%--------------------------------------------------------------------
dbg_get_state()-> 
    gen_server:call(?SERVER, {dbg_get_state},infinity).
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
    
    %% Connect nodes
    {ok,ConnecNodes}=etcd_infra:get_connect_nodes(?InfraSpecId),
    ConnectR=[{net_adm:ping(N),N}||N<-ConnecNodes],

    %% set cookie 
    {ok,CookieStr}=etcd_infra:get_cookie_str(?InfraSpecId),
    erlang:set_cookie(list_to_atom(CookieStr)),

    {ok,HostName}=net:gethostname(),
    {ok,NumNodes}=etcd_infra:get_num_workers(?InfraSpecId,HostName),
    WantedState=lib_node:create_wanted_state(NumNodes,CookieStr,HostName),
    
    
    DelDirR=[{R#node_record.node_dir,rpc:call(node(),file,del_dir_r,[R#node_record.node_dir],5000)}||R<-WantedState],
   % CreateStartNodesR=[lib_node:create_start_node(R)||R<-WantedState],
    
    CreateStartNodesR=lib_node:create_start_nodes(WantedState),
    NodeRecords=[R||{ok,R}<-CreateStartNodesR],
    
    IsWantedState=lib_node:is_wanted_state(WantedState),
    
    ?LOG_NOTICE("ConnectR ",[ConnectR]),
    ?LOG_NOTICE("DelDirR ",[DelDirR]),
    ?LOG_NOTICE("CreateStartNodesR ",[CreateStartNodesR]),
    ?LOG_NOTICE("IsWantedState Nodes ",[IsWantedState]),
   
    ?LOG_NOTICE("Server started ",[node()]),
    {ok, #state{
	    wanted_state=WantedState,   
	    wanted_state_status=IsWantedState,
	    node_records=NodeRecords
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



handle_call({kill}, _From, State) ->
    Reply=1/0,
    {reply, Reply, State};



handle_call({allocate}, _From, State) ->
    FreeNodeRecords=[NodeRecord||NodeRecord<-State#state.node_records,
				 free=:=NodeRecord#node_record.status],
    Reply = case FreeNodeRecords of
		[]->
		    NewState=State,
		    {error,["No available nodes ",?MODULE,?LINE]};
		[FreeNodRecord|_]->
		    L=lists:delete(FreeNodRecord,State#state.node_records),
		    R=FreeNodRecord#node_record{status=allocated,status_time={date(),time()}},
		    NewState=State#state{node_records=[R|L]},
		    {ok,R#node_record.allocated_id,R#node_record.node,R#node_record.node_dir}
	    end,
    {reply, Reply, NewState};

handle_call({free,AllocatedId}, _From, State) ->
    AllocatedRecord=[NodeRecord||NodeRecord<-State#state.node_records,
				 AllocatedId=:=NodeRecord#node_record.allocated_id],
    Reply = case AllocatedRecord of
		[]->
		    NewState=State,
		    {error,["No matched for allocate_id ",AllocatedId,?MODULE,?LINE]};
		[AllocatedNodRecord]->
		    L=lists:delete(AllocatedNodRecord,State#state.node_records),
		    slave:stop(AllocatedNodRecord#node_record.node),		    
		    R=AllocatedNodRecord#node_record{status=free,status_time={date(),time()}},
		    NewState=State#state{node_records=[R|L]},
		    ok
	    end,
    {reply, Reply, NewState};


handle_call({get_wanted_state}, _From, State) ->
    Reply=State#state.wanted_state,
    {reply, Reply, State};

handle_call({get_not_created}, _From, State) ->
    Reply=[NodeRecord||NodeRecord<-State#state.node_records,
		       not_created=:=NodeRecord#node_record.status],
    {reply, Reply, State};

handle_call({get_free}, _From, State) ->
    Reply=[NodeRecord||NodeRecord<-State#state.node_records,
		       free=:=NodeRecord#node_record.status],
    {reply, Reply, State};

handle_call({get_allocated}, _From, State) ->
    Reply=[NodeRecord||NodeRecord<-State#state.node_records,
		       allocated=:=NodeRecord#node_record.status],
    {reply, Reply, State};

handle_call({get_deleted}, _From, State) ->
    Reply=[NodeRecord||NodeRecord<-State#state.node_records,
		       deleted=:=NodeRecord#node_record.status],
    {reply, Reply, State};

handle_call({create_start_node,NodeName,NodeDir}, _From, State) ->
    Reply=case lib_node:create_start_node(NodeName,NodeDir) of
	      {error,Reason}->
		  NewState=State,
		  {error,Reason};
	      {ok,ProviderNode}->
		  R=#node_record{
		       allocated_id=os:system_time(microsecond),
		       nodename=NodeName,
		       node_dir=NodeDir,
		       status=free,    %free,allocated, stopped
		       status_time={date(),time()},   %when latest status was chenged
		       node=ProviderNode},
		  NewState=State#state{node_records=[R|State#state.node_records]},
		  {ok,ProviderNode,NodeDir}
	  end,
    {reply, Reply, NewState};

handle_call({stop_delete_node,AllocateId}, _From, State) ->
    NodeRecords=[NodeRecord||NodeRecord=#node_record{allocated_id=AllocateId}<-State#state.node_records],
    Reply=case NodeRecords of
	      []->
		  NewState=State,
		  {error,["Node record does note exists",AllocateId,?MODULE,?LINE]};
	      [NR]->
		  case lib_node:stop_delete_node(NR#node_record.node,NR#node_record.node_dir) of
		      {error,Reason}->
			  NewState=State,
			  {error,Reason};
		      ok->
			  NewState=State#state{node_records=lists:delete(NR,State#state.node_records)},
			  ok
		  end
	  end,
    {reply, Reply, NewState};

handle_call({is_alive,ProviderNode}, _From, State) ->
    Reply=lib_control_node:is_alive(ProviderNode),
    {reply, Reply, State};


handle_call({dbg_get_state}, _From, State) ->
    Reply=State,
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
    NodeRecordList=[NodeRecord||NodeRecord<-State#state.node_records,
				Node=:=NodeRecord#node_record.node],
    case NodeRecordList of
	[]->
	    NewState=State,
	    {error,["Node not part of cluster ",Node,?MODULE,?LINE]};
	[R]->
	    lib_node:stop_delete_node(R),
	    case  lib_node:create_start_node(R) of
		{error,Reason}->
		    NewState=State,
		    {error,Reason};
		{ok,UpdatedR}->
		   % NewState=State#state{node_records=[UpdatedR|lists:delete(R,State#state.node_records)]},
		    NewState=State#state{node_records=lists:reverse([UpdatedR|lists:delete(R,State#state.node_records)])},
		    ok
	    end
    end,
    io:format("DBGnodedown  Node, NodeRecordList, UpdateR  ~p~n",[{Node,NodeRecordList,[NodeRecord||NodeRecord<-NewState#state.node_records,
												    Node=:=NodeRecord#node_record.node],?MODULE,?LINE}]),
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
