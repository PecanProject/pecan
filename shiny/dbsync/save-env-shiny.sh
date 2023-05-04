#!/bin/sh

# save env
env > /home/shiny/.Renviron
chown shiny.shiny /home/shiny/.Renviron

# start shiny server
/usr/bin/shiny-server.sh
