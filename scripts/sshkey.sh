#!/bin/bash
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

# create ssh folder
if [ ! -d ~/.ssh ]; then mkdir ~/.ssh; fi
chmod 700 ~/.ssh

# get server and username
echo "Enter hostname for job submission and press [ENTER] :" 
read SERVER
if [ "$SERVER" == "" ]; then exit 0; fi

echo "Enter your username on ${SERVER} and press [ENTER] :" 
read USERNAME
if [ "$USERNAME" == "" ]; then exit 0; fi

# create ssh key and add to config file
if [ ! -e ~/.ssh/${SERVER} ]; then
  ssh-keygen -q -t rsa -N "" -f ~/.ssh/${SERVER}

  cat >> ~/.ssh/config << EOF

Host ${SERVER}
HostName ${SERVER}
User ${USERNAME}
IdentityFile ~/.ssh/${SERVER}
ServerAliveInterval 300
ControlMaster auto 
ControlPath /tmp/%r@%h:%p
EOF
  chmod 600 ~/.ssh/config
fi

# show key
echo "Please add ~/.ssh/${SERVER}.pub to your ~/.ssh/authorized_keys on $SERVER" 
echo "paste the following lines to the commandline:"
echo ""
echo " (the first only if there is no .ssh directory on the server)"
echo ""
echo "echo ssh ${USERNAME}@${SERVER} \"mkdir ~/.ssh\"" 
echo ""
echo "cat ~/.ssh/${SERVER}.pub | ssh ${USERNAME}@${SERVER} \"cat >> ~/.ssh/authorized_keys\"" 
