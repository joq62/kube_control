%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_control).
 

-include("control_config.hrl").

%% API
-export([
	 new_cluster/1,
	 delete_cluster/1,
	 deploy_application/2,
	 remove_application/3
	 
	]).

-export([
	 start_kubelet/2
	]).

%%%===================================================================
%%% API
%%%===================================================================


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Application  part Start
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%--------------------------------------------------------------------
%% @doc
%% Deploy an application ApplicationId on host that node  KubeletNode are
%% @end
%%--------------------------------------------------------------------
deploy_application(ApplicationId,KubeletNode)->
    {ok,ApplicationId,WorkerNode}=rpc:call(KubeletNode,kubelet,deploy_application,[ApplicationId],2*5000),
    {ok,ApplicationId,KubeletNode,WorkerNode}.

%%--------------------------------------------------------------------
%% @doc
%% Remove an application ApplicationId on host that node  KubeletNode are
%% @end
%%--------------------------------------------------------------------
remove_application(ApplicationId,KubeletNode,WorkerNode)->
    ok=rpc:call(KubeletNode,kubelet,remove_application,[ApplicationId,WorkerNode],2*5000),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Cluster part Start
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
new_cluster(HostNameNumWorkersList)->
    new_cluster(HostNameNumWorkersList,[]).

new_cluster([],Acc)->
    {ok,Acc};
new_cluster([{HostName,NumWorkers}|T],Acc) ->
    Timeout=NumWorkers*3000,
    Result=case rpc:call(node(),lib_control,start_kubelet,[HostName,NumWorkers],Timeout) of
	       {ok,Node}->
		   {ok,#{hostname=>HostName,node=>Node}};
	       {badrpc,Reason} ->
		   {error,["Error trying to start kubelet on Host ",HostName,NumWorkers,Reason,?MODULE,?LINE]}
	   end,
    new_cluster(T,[Result|Acc]).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
delete_cluster(KubeletList)->
    stop_kubelet(KubeletList,[]).

stop_kubelet([],Acc)->
    {ok,Acc};
stop_kubelet([{error,_}|T],Acc)->
    stop_kubelet(T,Acc);
stop_kubelet([{ok,KubeletInfo}|T],Acc)->
    KubeletNode=maps:get(node,KubeletInfo),
    rpc:call(KubeletNode,init,stop,[],5000),
    IsStopped=is_stopped(KubeletNode),
    stop_kubelet(T,[{IsStopped,KubeletInfo}|Acc]).


%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
start_kubelet(HostName,NumWorkers)->
    {HostName,Ip,Port,Uid,Pwd,_}=etcd_host:get_info(HostName),
    io:format("HostName,Ip,Port,Uid,Pwd ~p~n",[{HostName,Ip,Port,Uid,Pwd,?MODULE,?LINE}]),	
   				
    %start vm 
    {ok,HostName}=net:gethostname(),
    CookieStr=atom_to_list(erlang:get_cookie()),
    NodeName=?KubeletNodeName++"_"++CookieStr,
    Node=list_to_atom(NodeName++"@"++HostName),
    rpc:call(Node,init,stop,[],5000),
    true=is_stopped(Node),
  
    ErlCmd="erl -sname "++NodeName++" "++"-setcookie "++CookieStr++" "++"-noinput -detached",
    TimeOut=5000,
    {ok,[]}=ssh_service:send_msg(Ip,Port,Uid,Pwd,ErlCmd,TimeOut),
    true=is_started(Node),
  
    %% Start kubelet 
    {ok,[Home]}=ssh_service:send_msg(Ip,Port,Uid,Pwd,"pwd",TimeOut),
    Ebin=filename:join([Home,?KubeletDir,"ebin"]),
    true=rpc:call(Node,code, add_path,[Ebin],5000),
    ok=rpc:call(Node,application,load,[kubelet],5000),
    ok=rpc:call(Node,application,start,[kubelet],5000),
    pong=rpc:call(Node,kubelet,ping,[],5000),
    ok=create_workers(Node,NumWorkers),
    {ok,Node}.

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
create_workers(KubeletNode,NumWorkers)->
    Timeout=NumWorkers*3000,
    ok=rpc:call(KubeletNode,kubelet,create_workers,[NumWorkers],Timeout),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-define(NumTests,100).
-define(Interval,50).

is_started(Node)->
    is_started(Node,?NumTests,?Interval,false).

is_started(_,0,_,IsStarted)->
    IsStarted;
is_started(_,_,_,true) ->
    true;
is_started(Node,N,Interval,false) ->
    IsStarted=case net_adm:ping(Node) of
		  pong->
		      true;
		  pang ->
		      timer:sleep(Interval),
		      false
	      end,
    is_started(Node,N-1,Interval,IsStarted).
	
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
is_stopped(Node)->
    is_stopped(Node,?NumTests,?Interval,false).

is_stopped(_,0,_,IsStopped)->
    IsStopped;
is_stopped(_,_,_,true) ->
    true;
is_stopped(Node,N,Interval,false) ->
    IsStarted=case net_adm:ping(Node) of
		  pang->
		      true;
		  pong ->
		      timer:sleep(Interval),
		      false
	      end,
    is_stopped(Node,N-1,Interval,IsStarted).
	
