#!/bin/bash
#!/bin/bash
echo "sending jobs to cluster"
env PECANSETTINGS=$1 R --vanilla < ./rscripts/start.runs.R
wait
