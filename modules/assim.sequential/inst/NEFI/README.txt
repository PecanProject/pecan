Author: Luke Dramko

This collection of R scripts utilize the PEcAn workflow to iteratively run forecasts and do state data assimilation.

run.gefs.sipnet.sh, generate.gefs.xml.R, and gefs.sipnet.source.xml work in tandem
to run PEcAn's SIPNET model with current NOAA GEFS data.  This system can be put
on a cron job with minimal effort; simply put the target of the cron job as 
run.gefs.sipnet.sh

-- run.gefs.sipnet.sh --
This master bash script is intended to be called as a cron job, but it can be run
manually as well.  It first runs generate.gefs.xml.R, then runs PEcAn based on the
resulting xml.  If being used to manually run a PEcAn workflow, run.gefs.sipnet.sh's
first command line argument can be a date.  That date will be used as the start 
date for the run.  Several variables, declared at the top of the script, control
critical file paths, such as the location of the desired copy of workflow.R and 
the location of gefs.sipnet.source.xml and generate.gefs.xml

Note that the current version of run.gefs.sipnet.sh actually has EXAMPLE in the title.
This is because the script requires absolute paths to work properly, which will be different for every user.
These paths are listed as variables at the top of the file; simply change them to the appropriate paths on your machine.

-- gefs.sipnet.source.xml --
This is the basis xml file from which all workflows are based.  generate.gefs.xml.R
uses this as its basis.  Aside from the fields which generate.gefs.xml.R changes,
all changes to the xml file are conserved between runs.

-- generate.gefs.xml.R --
This script overwrites gefs.sipnet.source xml with fresh values for any given run.
It updates the following fields:
* start and end date for <run>
* workflow id
* time of the run (<pecan><info><date>)
* met.start and met.end
* start.year and end.year for <ensemble>
* output directory (but not the dbfile directory)
Additionally, this script does some important work usually done by the web interface.
In particular, it generates a unique workflow id based on the ids in the database
and insert the workflow ID.
It also generates the output folder.

Several other scripts are included to make gathering and interpeting data easier

-- graphs.R --
Generates a graph of NEE and LE calculated via a run with a 95% confidence interval
vs. observed data.  Can be called with either a date
or a workflow ID as a command line argument.
Note that the directory ./graphs/ must exist.

-- last12days.R --
This is a simple and very specific script that runs NOAA GEFS for the last 12
days.  This is useful because that's the range of time that NOAA_GEFS data is
avaliable.

* Database scripts *
These scripts are useful for modifying the database.  Because everything NOAA_GEFS 
does is repeated 21 times (1 for each ensemble member) it is impractical to 
clean the database via the web interface, where only one file can be removed at
a time. 


