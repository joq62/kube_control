%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created :  3 Dec 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(system_boot).

-behaviour(gen_server).


-include("control_config.hrl").
-include("log.api").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

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
    process_flag(trap_exit, true),
    timer:sleep(60*1000),

    %%-- Very dirty connect nodes , to ensure that etcd will start distriubted 
    
    Alive=[N||N<-?ConnectNodes,
	      pong=:=net_adm:ping(N)],
    
    Nodes=lists:append([rpc:call(N,erlang,nodes,[],5000)||{pong,N}<-Alive]),
    Nodes2=lists:usort(Nodes),
    ConnectResult=[{N1,N2,rpc:call(N1,net_adm,ping,[N2],5000)}||N1<-Nodes2,
								N2<-Nodes2,
								N1<N2],
    %    [{net_adm:ping(N),N}||N<-lists:usort(Nodes)],
   % ConnectResult=[{N1,N2,rpc:call(N1,net_adm,ping,[N2],5000)}||N1<-?ConnectNodes,
%								N2<-?ConnectNodes,
%								N1<N2],
    
    LogStart=rpc:call(node(),application,start,[log],2*5000),   
    %%-- init log
    [NodeName,_]=string:tokens(atom_to_list(node()),"@"),
    LogsAtHome=?MainLogDir,
    CreateLogFileResult=case filelib:is_dir(LogsAtHome) of
			     false->
				 case file:make_dir(LogsAtHome) of
				     {error,Reason}->
					 {error,[error,"Failed to make dir ",Reason,?MODULE,?LINE]};
				     ok ->
					 %%---------- create logger files
					 NodeNodeLogDir=filename:join(LogsAtHome,NodeName),
					 case log:create_logger(NodeNodeLogDir,?LocalLogDir,?LogFile,?MaxNumFiles,?MaxNumBytes) of
					     {error,Reason1}->
						 {error,[error,"Failed to create logger file ",Reason1,?MODULE,?LINE]};
					     ok ->
						 {ok,?LINE}
					 end
				 end;
			     true ->
				 %%---------- create logger files
				 NodeNodeLogDir=filename:join(LogsAtHome,NodeName),
				 case log:create_logger(NodeNodeLogDir,?LocalLogDir,?LogFile,?MaxNumFiles,?MaxNumBytes) of
				     {error,Reason1}->
					 {error,[error,"Failed to create logger file ",Reason1,?MODULE,?LINE]};
				     ok ->
					 {ok,?LINE}
				 end
			 end,
    
    RdStart=rpc:call(node(),application,start,[rd],2*5000),   
    EtcdStart=rpc:call(node(),application,start,[etcd],2*5000),   
   
    ?LOG_NOTICE("LogStart ",[{log,LogStart}]),
    ?LOG_NOTICE("RdStart ",[{rd,RdStart}]),
    ?LOG_NOTICE("EtcdStart ",[{etcd,EtcdStart}]),
    ?LOG_NOTICE("ConnectResult ",[ConnectResult]),
    ?LOG_NOTICE("CreateLogFileResult ",[CreateLogFileResult]),
    
    
    ?LOG_NOTICE("Server started ",[?MODULE]),
    
    {ok, #state{}}.

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
handle_info(_Info, State) ->
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
