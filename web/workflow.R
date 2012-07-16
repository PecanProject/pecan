#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
# ----------------------------------------------------------------------
# Load required libraries
# ----------------------------------------------------------------------
library(PEcAn.all)

# ----------------------------------------------------------------------
# initialization
# ----------------------------------------------------------------------
# load the pecan settings
settings <- read.settings("pecan.xml")

# remove existing STATUS file
file.remove("STATUS")

# ----------------------------------------------------------------------
# status functions
# ----------------------------------------------------------------------
status.start <- function(name) {
    cat(paste(name, format(Sys.time(), "%F %T"), sep="\t"), file="STATUS", append=TRUE)      
}
status.end <- function(status="DONE") {
    cat(paste("", format(Sys.time(), "%F %T"), status, "\n", sep="\t"), file="STATUS", append=TRUE)      
}

options(warn=1)
options(error=quote({
          status.end("ERROR")
          if (!interactive()) {
            q()
          }
        }))

#options(warning.expression=status.end("ERROR"))

# ----------------------------------------------------------------------
# status functions
# ----------------------------------------------------------------------
pecan.setup <- function(settings) {
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
                      paste('ssh -T ${REMOTEHOST} "mkdir -p', settings$run$host$rundir, settings$run$host$outdir, '"')),
                    con=paste(settings$outdir, 'setup.sh', sep=''))
        Sys.chmod(paste(settings$outdir, 'setup.sh', sep=''), mode = "0755")
        
        # running using qsub
        script <- gsub('@CMD@',  'QSUB=$( qsub -cwd -N $f -pe mpich 1 -j y -o $LOG ./runjob.sh $f );', script)
        script <- gsub('@ID@',   'ID=${QSUB#*Your job }; ID=${ID%% *};', script)
        
        # write script to disk
        writeLines(script, con=paste(settings$outdir, 'qsub.sh', sep=''))
        Sys.chmod(paste(settings$outdir, 'qsub.sh', sep=''), mode = "0755")
    
        # actual command to start ed
        writeLines(c( '#!/bin/bash',
                      paste('mpirun -np 1', settings$model$binary, '-f $1'),
                      paste('rsync -routi /scratch/out*', settings$run$host$outputs)),
                   con=paste(settings$outdir, 'runjob.sh', sep='/'))
        Sys.chmod(paste(settings$outdir, 'runjob.sh', sep=''), mode = "0755")
           
        # launcher used to start process
        writeLines(c( '#!/bin/bash',
                      paste('rsync -outip ', settings$outdir, '*.sh ', settings$run$host$name, ':', settings$run$host$rundir, ' 2>&1 >/dev/null', sep=''),
                      paste('ssh -T ', settings$run$host$name, ' ', settings$run$host$rundir, 'qsub.sh', sep='')),
                   con=paste(settings$outdir, 'launcher.sh', sep='/'))
        Sys.chmod(paste(settings$outdir, 'launcher.sh', sep=''), mode = "0755")
           
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
                   con=paste(settings$outdir, 'check.sh', sep='/'))
        Sys.chmod(paste(settings$outdir, 'check.sh', sep=''), mode = "0755")
    } else {
        # write script to create tunnel and folders
        writeLines(c( '#!/bin/bash',
                      paste('mkdir -p', settings$run$host$rundir),
                      paste('mkdir -p', settings$run$host$outdir)),
                   con=paste(settings$outdir, 'setup.sh', sep='/'))
        Sys.chmod(paste(settings$outdir, 'setup.sh', sep=''), mode = "0755")
           
        # actual command to start ed
        script <- gsub('@CMD@',  paste(settings$model$binary, '-f $f'), script)
        script <- gsub('@ID@',   'ID=""', script)
        
        # write script to disk
        writeLines(script, con=paste(settings$outdir, 'launcher.sh', sep='/'))
        Sys.chmod(paste(settings$outdir, 'launcher.sh', sep=''), mode = "0755")
    
        # write check script to disk
        writeLines(c( '#!/bin/bash',
                      '/bin/true' ),
                   con=paste(settings$outdir, 'check.sh', sep='/'))
        Sys.chmod(paste(settings$outdir, 'check.sh', sep=''), mode = "0755")
    }
}

# ----------------------------------------------------------------------
# run workflow
# ----------------------------------------------------------------------
status.start("SETUP")
pecan.setup(settings)
system(paste(settings$outdir, 'setup.sh', sep='/'))
status.end()

# setup pss/css by running fia2ED
status.start("FIA2ED")
# TODO see if we need to call fia
#if (names(settings$run$site$psscss) %in% ".attrs") {
#  if (settings$run$site$psscss$.attrs[["generate"]] == "fia") {
#    fia.to.psscss(settings)
#  } else {
#    stop("No inforamtion on how to generate psscss files.")
#  }
#}
status.end()

# get data from pecan DB
status.start("TRAIT")
get.trait.data()
status.end()

# run meta-analysis
status.start("META")
run.meta.analysis()
status.end()

# write model specific configs
status.start("CONFIG")
run.write.configs(settings$model$name)
status.end()

# run model
status.start("MODEL")
# write model started
con <- query.base.con(settings)
query.base(paste("INSERT INTO runs (site_id, start_time, finish_time, outdir, created_at, started_at) values ('", settings$run$site$id, "', '", settings$run$start.date, "', '", settings$run$end.date, "', '",settings$outdir , "', NOW(), NOW())", sep=''), con)
id <- query.base(paste("SELECT LAST_INSERT_ID() AS ID"), con)
oldwd <- getwd()
if (settings$model$name == "ED2") {
  setwd(settings$outdir)
  system2(paste(settings$outdir, 'launcher.sh', sep=''), wait=TRUE)
} else if (settings$model$name == "SIPNET") {
  setwd(paste(settings$outdir, "SAmedian", sep="/"))
  system2(settings$model$binary, wait=TRUE)
}
setwd(oldwd)
# TODO need to add code to check if remote jobs are done
#system2(paste(settings$outdir, 'check.sh', sep=''), wait=TRUE)
# write model finished
query.base(paste("UPDATE runs SET finished_at =  NOW() WHERE id = ", id), con)
query.close(con)
status.end()

# all done
status.start("FINISHED")
status.end()

