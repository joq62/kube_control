%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 31 Jul 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_node).


-include("node_record.hrl").
-include("log.api").

-define(MainLogDir,"logs").
-define(LocalLogDir,"to_be_changed.logs").
-define(LogFile,"logfile").
-define(MaxNumFiles,10).
-define(MaxNumBytes,100000).

-define(Iterations,100).
-define(ControlAppFile,"control.app").
%% API
-export([
	 create_wanted_state/3,
	 is_wanted_state/1,

	 create_start_nodes/1,
	 create_start_node/1,
	 stop_delete_node/1,
	 
	 is_node_stopped/1,
	 is_node_started/1,
	 is_alive/1
	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
create_wanted_state(NumNodes,CookieStr,HostName)->
    create_wanted_state(NumNodes,CookieStr,HostName,[]).

create_wanted_state(0,_CookieStr,_HostName,WantedState)->
    WantedState;
create_wanted_state(N,CookieStr,HostName,Acc) ->
    NStr=integer_to_list(N),						
    NodeName=NStr++"_"++CookieStr,
    NodeDir=NStr++"_"++CookieStr,
    Node=list_to_atom(NodeName++"@"++HostName),
    R=#node_record{
	 allocated_id=os:system_time(nanosecond),
	 hostname=HostName,
	 nodename=NodeName,
	 node_dir=NodeDir,
	 cookie_str=CookieStr,
	 status=not_created,        % free,allocated, not_created, deleted
	 status_time={date(),time()},   % when latest status was changed
	 node=Node},
    create_wanted_state(N-1,CookieStr,HostName,[R|Acc]).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
create_start_nodes(WantedState)->
    create_start_nodes(WantedState,[]).
create_start_nodes([],Acc)->
    Acc;
create_start_nodes([R|T],Acc) ->
    NewAcc=case create_start_node(R) of
	       {error,Reason}->
		   ?LOG_WARNING("Error when creating a node R  ",[R,Reason]), 
		   Acc;
	       {ok,NewR} ->
		   ?LOG_NOTICE("Succeded to create a node R  ",[NewR]), 
		   [{ok,NewR}|Acc]
	   end,
    create_start_nodes(T,NewAcc).

create_start_node(R)->
    HostName=R#node_record.hostname,
    NodeName=R#node_record.nodename,
    NodeDir=R#node_record.node_dir,
    CookieStr=R#node_record.cookie_str,
    Result=case file:make_dir(NodeDir) of
	       {error,Reason}->
		   {error,["Failed to created Dir",NodeDir,Reason,?MODULE,?LINE]};
	       ok->
		   ErlArgs=" -setcookie "++CookieStr,
		   case slave:start(HostName,NodeName,ErlArgs) of
		       {error,{already_running, ProviderNode}}->
			   {error,["Already running ",?MODULE,?LINE,ProviderNode,NodeName,CookieStr,ErlArgs]};
		       {error,Reason}->
			   {error,["Failed to start Node ",?MODULE,?LINE,Reason,NodeName,CookieStr,ErlArgs]};
		       {ok,ProviderNode}->	
			   %% start log and resource_discovery that are in controls ebin
			   case code:where_is_file("log.app") of
			       non_existing ->
				   {error,["non_existing ",?ControlAppFile,?MODULE,?LINE]}; 
			       ShortPath ->
				   {ok,PathToControl}=file:get_cwd(),
				   EbinPathToLogApp=filename:join(PathToControl,ShortPath),
				   EbinPath=filename:dirname(EbinPathToLogApp),
				   case rpc:call(ProviderNode,code,add_patha,[EbinPath],5000) of
				       {error, bad_directory}->
					   {error,["bad_directory ",EbinPath,?MODULE,?LINE]};
				       true ->
					   case rpc:call(ProviderNode,application,start,[log],5000) of
					       {error,Reason}->
					  	   {error,["Failed to start log ",Reason,?MODULE,?LINE]};
					       ok->
					  	   LocalLogDir=atom_to_list(ProviderNode)++".logs",
					  	   case rpc:call(ProviderNode,log,create_logger,[?MainLogDir,LocalLogDir,?LogFile,?MaxNumFiles,?MaxNumBytes],5000) of
					  	       {error,Reason}->
					  		   {error,["Failed to start log ",Reason,?MODULE,?LINE]};
					  	       ok->
							   case rpc:call(ProviderNode,application,start,[rd],5000) of
							       {error,Reason}->
								   {error,["Failed to start rd ",Reason,?MODULE,?LINE]};
							       ok->
								   erlang:monitor_node(ProviderNode,true),
								   {ok, R#node_record{ allocated_id=os:system_time(nanosecond),status=free,status_time={date(),time()},node=ProviderNode}}
							   end
						   end
					   end
				   end
			   end
		   end
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
stop_delete_node(R)->
    Node=R#node_record.node,
    NodeDir=R#node_record.node_dir,
    Result=case file:del_dir_r(NodeDir) of
	       {error,Reason}->
		   slave:stop(Node),
		   {error,["Failed to delete Dir",NodeDir,Reason,?MODULE,?LINE]};
	       ok->
		   slave:stop(Node),
		   {ok,R#node_record{status=deleted,status_time={date(),time()}}}
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_alive(Node)->
    case is_node_started(Node) of
	false->
	    false;
	true ->
	    true
    end.
								 
%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_wanted_state(WantedState)->
    is_wanted_state(WantedState,[]).

is_wanted_state([],[])->
    true;
is_wanted_state([],WhyNot) ->
    {false,WhyNot};
is_wanted_state([R|T],Acc)->
    Ping=net_adm:ping(R#node_record.node),
    IsDir=filelib:is_dir(R#node_record.node_dir),
    NewAcc=case {Ping,IsDir} of
	       {pong,true}->
		   Acc;
	       _ ->
		   [{false,[{R#node_record.node,Ping},{R#node_record.node_dir,IsDir}]}|Acc]
	   end,
    is_wanted_state(T,NewAcc).
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_node_started(Node)->
    is_node_started(?Iterations,Node,false).

is_node_started(_N,_Node,true)->
    true;
is_node_started(0,_Node,Boolean) ->
    Boolean;
is_node_started(N,Node,_) ->
  %  io:format(" ~p~n",[{N,Node,erlang:get_cookie(),?MODULE,?LINE}]),
    Boolean=case net_adm:ping(Node) of
		pang->
		    timer:sleep(30),
		    false;
		pong->
		    true
	    end,
    is_node_started(N-1,Node,Boolean).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_node_stopped(Node)->
    is_node_stopped(?Iterations,Node,false).

is_node_stopped(_N,_Node,true)->
    true;
is_node_stopped(0,_Node,Boolean) ->
    Boolean;
is_node_stopped(N,Node,_) ->
    Boolean=case net_adm:ping(Node) of
		pong->
		    timer:sleep(500),
		    false;
		pang->
		    true
	    end,
    is_node_stopped(N-1,Node,Boolean).
