# ----------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------
library(XML)
if(interactive()){
  user <- Sys.getenv('USER')
  if(user == 'dlebauer'){
    settings.file = '~/pecan/settings.pavi.xml'
  } else if(user == 'davids14') {
    settings.file = '~/pecan/tundra.xml'
  } else {
    paste('please specify settings file in meta.analysis.R')
  }
} else {
  settings.file <- Sys.getenv('PECANSETTINGS')
}

settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)

# ----------------------------------------------------------------------
# CREATE FOLDERS
# ----------------------------------------------------------------------
system(paste('mkdir -p', settings$outdir))
system(paste('mkdir -p', settings$pfts$pft$outdir))

# ----------------------------------------------------------------------
# WRITE LAUNCH SCRIPTS
# ----------------------------------------------------------------------
# script that will either launch ed or do a qsub
script <- c("#!/bin/bash",
            paste("cd", settings$run$host$rundir),
            "JOBS=\"\"",
            "export GFORTRAN_UNBUFFERED_ALL=1",
            "for f in ED2INc*[0-9n]; do",
            "  LOG=\"$f.log\"",
            "  date +%Y.%m.%d-%H.%M > $LOG",
            "  @CMD@ >> $LOG",
            "  @ID@",
            "  JOBS=\"$JOBS $ID\"",
            "done",
            "echo $JOBS")

# figure out if we run local, or using qsub
if ( settings$run$host$name != "" && settings$run$host$name != "localhost" ) {
  # write script to create tunnel and folders
  writeLines(c( '#!/bin/bash',
                paste('REMOTEHOST=', settings$run$host$name, sep=''),
                'ssh -Nf ${REMOTEHOST}',
                'REMOTEPID=$( ps -ef | grep "ssh" | grep "${REMOTEHOST}" | awk \'{ print $2 }\' )',
                'echo ${REMOTEPID} > ssh.pid',
                paste('ssh -T ${REMOTEHOST} "mkdir -p', settings$run$host$rundir, '"'),
                paste('ssh -T ${REMOTEHOST} "mkdir -p', settings$run$host$outdir, '"')),
             con=paste(settings$outdir, 'setup.sh', sep=''))

  # running using qsub
  script <- gsub('@CMD@',  'QSUB=$( qsub -cwd -N $f -pe mpich 1 -j y -o $LOG ./runjob.sh $f );', script)
  script <- gsub('@ID@',   'ID=${QSUB#*Your job }; ID=${ID%% *};', script)

  # write script to disk
  writeLines(script, con=paste(settings$outdir, 'qsub.sh', sep=''))

  # actual command to start ed
  writeLines(c( '#!/bin/bash',
                paste('mpirun -np 1', settings$run$host$ed$binary, '-f $1'),
                paste('rsync -routi /scratch/out*', settings$run$host$outputs)),
             con=paste(settings$outdir, 'runjob.sh', sep=''))

  # launcher used to start process
  writeLines(c( '#!/bin/bash',
                paste('rsync -outip ', settings$outdir, '*.sh ', settings$run$host$name, ':', settings$run$host$rundir, ' 2>&1 >/dev/null', sep=''),
                paste('ssh -T ', settings$run$host$name, ' ', settings$run$host$rundir, 'qsub.sh', sep='')),
             con=paste(settings$outdir, 'launcher.sh', sep=''))

  # write check script to disk
  writeLines(c( '#!/bin/bash',
                'JOBS=$( cat jobs )',
                'echo "Checking $JOBS"',
                'while [ "$JOBS" != "" ]; do',
                '  sleep 30',
                '  LEFT=""',
                '  for j in $JOBS; do',
                paste('    if [ "`ssh -T', settings$run$host$name, 'qstat -j $j 2>&1 | wc -l`" == 2 ]; then'),
                '      echo "JOB $j is finished."',
                '    else',
                '      LEFT="$LEFT $j"',
                '    fi',
                '  done',
                '  JOBS=$LEFT',
                'done'),
             con=paste(settings$outdir, 'check.sh', sep=''))
} else {
  # write script to create tunnel and folders
  writeLines(c( '#!/bin/bash',
                paste('mkdir -p', settings$run$host$rundir),
                paste('mkdir -p', settings$run$host$outdir)),
             con=paste(settings$outdir, 'setup.sh', sep=''))

  # actual command to start ed
  script <- gsub('@CMD@',  paste(settings$run$host$ed$binary, '-f $f'), script)
  script <- gsub('@ID@',   'ID=""', script)

  # write script to disk
  writeLines(script, con=paste(settings$outdir, 'launcher.sh', sep=''))

  # write check script to disk
  writeLines(c( '#!/bin/bash',
                '/bin/true' ),
             con=paste(settings$outdir, 'check.sh', sep=''))
}
