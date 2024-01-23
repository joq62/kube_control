%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(control_provider_server).
 
-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").
 

%% API

-export([
	 set_wanted_state/1,
	 get_deployments/0,
	 load_provider/1,
	 start_provider/1,
	 stop_provider/1,
	 unload_provider/1,
	 is_alive/1,

	 ping/0,
	 stop/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {
		is_deployed,
		wanted_state,
		deployments,
		monitored_nodes
	       }).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec get_deployments() -> DeploymentList :: term().

get_deployments()->
    gen_server:call(?SERVER, {get_deployments},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec set_wanted_state(DeploymentSpec :: string()) -> ok | {error, Error :: term()}.

set_wanted_state(DeploymentSpec)->
    gen_server:call(?SERVER, {set_wanted_state,DeploymentSpec},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec load_provider(DeploymentRecord :: term()) -> ok | {error, Error :: term()}.

load_provider(DeploymentRecord)->
    gen_server:call(?SERVER, {load_provider,DeploymentRecord},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec start_provider(DeploymentRecord :: term()) -> ok | {error, Error :: term()}.

start_provider(DeploymentRecord)->
    gen_server:call(?SERVER, {start_provider,DeploymentRecord},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec stop_provider(DeploymentRecord :: term()) -> ok | {error, Error :: term()}.

stop_provider(DeploymentRecord)->
    gen_server:call(?SERVER, {stop_provider,DeploymentRecord},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec unload_provider(DeploymentRecord :: term()) -> ok | {error, Error :: term()}.

unload_provider(DeploymentRecord)->
    gen_server:call(?SERVER, {unload_provider,DeploymentRecord},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Get all information related to host HostName  
%% @end
%%--------------------------------------------------------------------
-spec is_alive(DeploymentRecord :: term()) -> IsDeployed :: boolean() | {error, Error :: term()}.

is_alive(DeploymentRecord)->
    gen_server:call(?SERVER, {is_alive,DeploymentRecord},infinity).


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


stop()-> gen_server:call(?SERVER, {stop},infinity).

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
    
    ?LOG_NOTICE("Server started ",[]),
    {ok, #state{is_deployed=false,
		monitored_nodes=[]}}.


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

handle_call({set_wanted_state,_DeploymentSpec}, _From, State) when State#state.is_deployed=:=true ->
    io:format("set_wanted_state true ~p~n",[{?MODULE,?LINE}]),
    Reply={error,["Wanted State is already deployed ",?MODULE,?LINE]},
    {reply, Reply, State};

handle_call({set_wanted_state,DeploymentSpec}, _From,   State) when State#state.is_deployed=:=false ->
    Reply=case lib_provider:set_wanted_state(DeploymentSpec) of
	      {error,Reason}->
		  NewState=State,
		  {error,Reason};
	      {ok,{Successfull,Failed}}->
		  io:format("Successfull,Failed ~p~n",[{Successfull,Failed,?MODULE,?LINE}]),
		  case Failed of
		      []->
			  ok;
		      Failed->
			  ?LOG_WARNING("Failed to load start ",[Failed])
		  end,
		  NewNodes=[Node||{_Id,Node,_NodeDir,_Provider,_App}<-Successfull],
		  NewNodesToMonitor=[Node||Node<-NewNodes,
					   false=:=lists:member(Node,State#state.monitored_nodes)],
		  [erlang:monitor_node(Node,true)||Node<-NewNodesToMonitor],
		  NodesToMonitor=lists:usort(lists:append(State#state.monitored_nodes,NewNodesToMonitor)),		  
		  NewState=State#state{
			     is_deployed=true,
			     wanted_state=DeploymentSpec,
			     deployments=Successfull,
			     monitored_nodes=NodesToMonitor},
		  ok
	  end,
    {reply, Reply, NewState};

handle_call({load_provider,Provider}, _From, State) ->
    Reply=case lib_provider:load(Provider) of 
	      {error,Reason}->
		  NewState=State,
		  {error,[Provider,Reason]};
	      {ok,Id,Node,NodeDir,Provider,App}->
		  case lists:member(Node,State#state.monitored_nodes) of
		      false->
			  erlang:monitor_node(Node,true),
			  NewState=State#state{deployments=[{Id,Node,NodeDir,Provider,App}|State#state.deployments],
					       monitored_nodes=[Node|State#state.monitored_nodes]},
			  {ok,Id};
		      true->
			  NewState=State#state{deployments=[{Id,Node,NodeDir,Provider,App}|State#state.deployments]},
			  {ok,Id}
		  end
	  end,
    {reply, Reply, NewState};

handle_call({unload_provider,Id}, _From, State) ->
    Reply=case lists:keyfind(Id,1,State#state.deployments) of
	      false->
		  NewState=State,
		  {error,["Id doesnt exists ",Id]};
	      {Id,Node,NodeDir,_Provider,App}->
		  case lib_provider:unload(Id,Node,NodeDir,App) of
		      {error,Reason}->
			  NewState=State,
			  {error,Reason};
		      ok->
			  erlang:monitor_node(Node,false),
			  NewState=State#state{deployments=lists:keydelete(Id,1,State#state.deployments),
					       monitored_nodes=lists:delete(Node,State#state.monitored_nodes)},
			  ok
		  end
	  end,
    {reply, Reply, NewState};

handle_call({start_provider,Id}, _From, State) ->
    Reply=case lists:keyfind(Id,1,State#state.deployments) of
	      false->
		  {error,["Id doesnt exists ",Id]};
	      {_Id,Node,_NodeDir,_Provider,App}->
		  lib_provider:start(Node,App)
	  end,
    {reply, Reply, State};

handle_call({stop_provider,Id}, _From, State) ->
    Reply=case lists:keyfind(Id,1,State#state.deployments) of
	      false->
		  {error,["Id doesnt exists ",Id]};
	      {_Id,Node,_NodeDir,_Provider,App}->
		  lib_provider:stop(Node,App)
	  end,
    {reply, Reply, State};



handle_call({is_alive,Id}, _From, State) ->
    Reply=case lists:keyfind(Id,1,State#state.deployments) of
	      false->
		  {error,["Id doesnt exists ",Id]};
	      {_Id,Node,_NodeDir,_Provider,App}->
		  lib_provider:is_alive(Node,App)
	  end,
    {reply, Reply, State};

handle_call({get_deployments}, _From, State) ->
    Reply=State#state.deployments,
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

%% Monitored Node down
%% Stop monitoring that node 
%% Remove from  State#state.monitored_nodes
%% Get the deployment on that node 
%% Remove Deployment from deployment list
%% 

handle_info({nodedown,Node}, State) ->
    io:format("nodedown ~p~n",[{Node,?MODULE,?LINE}]),
    erlang:monitor_node(Node,false),
    case lists:keyfind(Node,2,State#state.deployments) of
	false->
	    io:format("error ~p~n",[{"eexists Node ",Node,?MODULE,?LINE}]),
	    NewState=State#state{monitored_nodes=lists:delete(Node,State#state.monitored_nodes)},
	    {error,["eexists Node ",Node,?MODULE,?LINE]};
	{Id,Node,NodeDir,Provider,App}->
	    io:format("ok  ~p~n",[{?MODULE,?LINE}]),
	    L1=lists:delete({Id,Node,NodeDir,Provider,App},State#state.deployments),
	    case lib_provider:load_start(Provider) of
		{error,Reason}->
		    io:format("error  ~p~n",[{Reason,?MODULE,?LINE}]),
		    NewState=State#state{deployments=L1,
					 monitored_nodes=lists:delete(Node,State#state.monitored_nodes)};
		{ok,NewId,NewNode,NewNodeDir,Provider,App}->
		    io:format("ok NewId,NewNode,NewNodeDir,Provider,App ~p~n",[{NewId,NewNode,NewNodeDir,Provider,App,?MODULE,?LINE}]),
		    erlang:monitor_node(NewNode,true),
		    NodesToMonitor=lists:usort([Node|State#state.monitored_nodes]),		  
		    NewState=State#state{deployments=[{NewId,NewNode,NewNodeDir,Provider,App}|L1],
					 monitored_nodes=NodesToMonitor}
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
