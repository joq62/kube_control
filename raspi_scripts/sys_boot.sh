#!/bin/bash
/usr/bin/erl -pa /home/ubuntu/control/ebin -sname control_a  -setcookie a -run control start -noinput &
echo "$!" > /run/sys_boot.pid
