These database scripts help identify and modify the state of the BETY database.
Working with NOAA_GEFS data produces a lot of database files and IDs, so 
this is a nicer way to interact with them all at once, rather than through
the web interface.

-- dbsetup.R --
Sets up the connection to the database for running interactively in the consonle
sourcing this script produces a PostgreSQL connection object named con, which
can be passed to any of the PEcAn.DB functions.  Remember to close the connection
when you're done with it.

-- dbclean.R --
Completely wipes the database of all NOAA_GEFS data at site 676 (Willow Creek)
The script will print out all of the files it wants to wipe first, and exit.
Run with the command line argument TRUE in order to clear files.

-- dbSelectRemove.R --
dbSelectRemove is a more conservative version of dbclean.  It removes only dbfiles
and input files from a given day, provided as a command line argument.  By default,
it will then print each file and exit; run with TRUE as a second command line a
rgument to delete the files.  dbSelectRemove.R doesn NOT remove input/dbfiles
for raw data, only for gapfilled and model-converted data.
