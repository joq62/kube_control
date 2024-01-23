#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa control/ebin -setcookie a -sname install_test

main([])->
      {ok,HostName}=net:gethostname(),
      Node=list_to_atom("control_a@"++HostName),
      rpc:call(Node,init,stop,[],5000),
      timer:sleep(3000),
      file:del_dir_r("control"),
      file:del_dir_r("logs"),
      os:cmd("rm -r *_a"),
      os:cmd("git clone https://github.com/joq62/control.git"),
      io:format("Installed ~n").
