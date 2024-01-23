#!/bin/bash
erl -pa control/ebin -sname control_a  -setcookie a -run control start;
echo Eagle has wings
