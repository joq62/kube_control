-define(KubeletNodeName,"kubelet").
-define(KubeletDir,"kubelet").



-define(DeploymentSpec,"permanent").
-define(InfraSpec,"basic").

-define(ConnectNodes,[control_a@c50,
		      control_a@c200,
		      control_a@c201,
		      control_a@c202,
		      control_a@c230
		     ]).

-define(MaxDetectTime,60*1000*1).

-define(MainLogDir,"logs").
-define(LocalLogDir,"log.logs").
-define(LogFile,"logfile").
-define(MaxNumFiles,10).
-define(MaxNumBytes,100000).
