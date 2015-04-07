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

# add ebi-cluster to known_hosts
if [ ! -e ~/.ssh/known_hosts ]; then
  if [ "$SERVER" == "ebi-cluster" -o "$SERVER" == "ebi-cluster.igb.uiuc.edu" -o "$SERVER" == "ebi-cluster.igb.illinois.edu" ]; then
    cat >> ~/.ssh/known_hosts << EOF
|1|l+VkFK7zeYBwudLstZfm37DK48E=|MQDja2Ro4KoY2dMz45D5GPKPS8M= ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAnrdRRv7yzYLyb5Xqny6ZCecd37E/Wab76wNGnqoRB8JxFmG0GjpfPY0GeOhRMK7Mph9ZJdX8Fc+rZh64TMUJVUOLT0lQhxBfiFEXM3mRJKcwBD8qoYCwvUlegjeYH53+wa3wXnuWLJaCLL6yOtyHyEXI1RR7Gc124f39uMBAOjPqO4UsbrrplkjC+gzPTeRd8F0J+qdHvakdKIDlYrqHalIJjTLcvcOMIXft3yu7Uvet1c1fhtzy63pZTc485x1GMLgDI+aELYk2A76ZxHa7M6RSL0dCifs33SjorIYsU7EdwaCGvpH0CdTvxHTagQzGA44liWeGA4l2pV5lx+KOqQ==
|1|xR8lfSy1VnfuBhkIQ7i//qa3TUY=|cYqjphhu4IG9DCac/PXhiyZgVUI= ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAnrdRRv7yzYLyb5Xqny6ZCecd37E/Wab76wNGnqoRB8JxFmG0GjpfPY0GeOhRMK7Mph9ZJdX8Fc+rZh64TMUJVUOLT0lQhxBfiFEXM3mRJKcwBD8qoYCwvUlegjeYH53+wa3wXnuWLJaCLL6yOtyHyEXI1RR7Gc124f39uMBAOjPqO4UsbrrplkjC+gzPTeRd8F0J+qdHvakdKIDlYrqHalIJjTLcvcOMIXft3yu7Uvet1c1fhtzy63pZTc485x1GMLgDI+aELYk2A76ZxHa7M6RSL0dCifs33SjorIYsU7EdwaCGvpH0CdTvxHTagQzGA44liWeGA4l2pV5lx+KOqQ==
EOF
  fi
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
