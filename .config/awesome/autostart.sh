#!/bin/sh

run() {
  if ! pgrep -f "$1" ;
  then
    "$@"&
  fi
}
run "xfce4-power-manager --daemon"
run "caffeine"
