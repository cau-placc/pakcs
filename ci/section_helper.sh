#!/bin/bash

#
# Takes 2 Parameters a new section id and a heading/title
#
function start_section() {
  id=$1
  title=$2
  echo -e "\e[0Ksection_start:$(date +%s):${id}[collapsed=true]\r\e[0K${title}"
}

#
# Takes 1 Parameter, the unique section id of the section that should end
#
function end_section() {
  id=$1
  echo -e "\e[0Ksection_end:$(date +%s):${id}\r\e[0K"
}