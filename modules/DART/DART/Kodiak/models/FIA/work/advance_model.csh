#!/bin/csh
#
# DART software - Copyright 2004 - 2011 UCAR. This open source software is
# provided by UCAR, "as is", without charge, subject to all terms of use at
# http://www.image.ucar.edu/DAReS/DART/DART_download
#
# $Id: advance_model.csh 4945 2011-06-02 22:29:30Z thoar $
#
# This is a modified standard script for use in assimilation applications
# where the model advance is executed as a separate process.
# It can be used with ED2 or other 0-dimensional forest models. It should also
# work with grid models, but hasn't been tested with those yet.
# 
# Arguments are (created by 'filter' or 'perfect_model_obs' and include):
# 1) the process number of caller,
# 2) the number of ensemble members/state copies belonging to that process, and 
# 3) the name of the control_file for that process.
# 
# If this script finishes and the 'control_file' still exists, it is
# an ERROR CONDITION and means one or more of the ensemble members did
# not advance properly. Despite our best attempts to trap on this
# condition, some MPI installations simply hang, some properly terminate.
#
# This script loops over all the entries in the control_file to advance 
# any/all of the ensemble members.  The number of trips through the 
# loop is the second argument to this script. The control_file contains 
# the information about which ensemble members are to be advanced by THIS TASK.
# Sometimes it may be just one ensemble member, sometimes all of them.
# Read DART/doc/html/filter_async_modes.html and the mpi_intro.html
# for an overview.
#
# This script has 4 logical 'blocks':
# 1) creates a clean, temporary directory in which to run a model instance
#    and copies the necessary files into the temporary directory
# 2) copies/converts the DART state vector to something the model can ingest
# 3) runs the model
# 4) copies/converts the model output to input expected by DART

set      process = $1
set   num_states = $2
set control_file = $3

#----------------------------------------------------------------------
# Block 1: copy necessary input files/executables/files common
#          to all model advances to a clean, temporary directory.
#          These will be used by ALL of the ensemble
#          members being advanced by this script.
#----------------------------------------------------------------------

# Create a unique temporary working directory for this process's stuff
# The run-time directory for the entire experiment is called CENTRALDIR;
# we need to provide a safe haven for each TASK ... in 'temp_dir'.

set temp_dir = 'advance_temp'${process}

# Create a clean temporary directory and go there
\rm -rf  $temp_dir  || exit 1
mkdir -p $temp_dir  || exit 1
cd       $temp_dir  || exit 1

# Get the necessary programs for ED2 run. Here:
# input.nml = the DART namelist.
# T_ED2IN = Template for the ED2 namelist. Only the dates are changed in it.
# ed_2.1-opt = The executive file for ED2.
# F2R = Fortran program, that reads the state vector from the binary data and writes
#    it out in ascii for for the R programs. Also writes out the names of the history files necessary for the run.
#    IMPORTANT: Should be compiled with gfortran.
# R2F = Fortran program that writes the advanced model values to the state vector for DART.
#    IMPORTANT: Should be compiled with gfortran.
# RH_B.R = R program that modifies the history file to contain the state vector values.
# H_B.R = R program that read the advanced state vector values from the history file.
# createInput.R = R program that writes the ED2 namelist file ED2IN.
cp -f ../input.nml        .  || exit 1
cp -f ../T_ED2IN          .  || exit 1
cp -f ../Raczka.xml       .  || exit 1
cp -f ../P2B.csv          .  || exit 1
ln -s ../ed_2.1-opt       .  || exit 1
ln -s ../F2R              .  || exit 1
ln -s ../R2F              .  || exit 1
ln -s ../createInput.R    .  || exit 1
ln -s ../H_B.R       .  || exit 1
ln -s ../RH_B.R      .  || exit 1

# Loop through each state
set state_copy = 1
set ensemble_member_line = 1
set      input_file_line = 2
set     output_file_line = 3

while($state_copy <= $num_states)
   
   set ensemble_member = `head -$ensemble_member_line ../$control_file | tail -1`
   set input_file      = `head -$input_file_line      ../$control_file | tail -1`
   set output_file     = `head -$output_file_line     ../$control_file | tail -1`
   
   #-------------------------------------------------------------------
   # Block 2: copy/convert the DART state vector to something the 
   #          model can ingest. In this case, just copy.
   #          In general, there is more to it. 
   #
   #          * copy/link ensemble-member-specific files
   #          * convey the advance-to-time to the model
   #          * convert the DART state vector to model format 
   #-------------------------------------------------------------------

   # Moves the ensemble member containing the relevant state vector to the temporary directory
   mv ../$input_file temp_input.dat || exit 2
   # Runs the program that converts the state vector in to ascii form for R programs to use and names
   # the relevant history files.
   ./F2R

    cat 4Rvalues.dat

   # Reads the name of the relevant history file.
   set file_name = `head file_name.txt`
   set start_date = `head start_date`

   if(-e /projectnb/dietzelab/DART_analy/analy_$ensemble_member) then
   else
	mkdir /projectnb/dietzelab/DART_analy/analy_$ensemble_member
   endif

   if(-e /projectnb/dietzelab/DART_analy/histo_$ensemble_member) then
   else
	mkdir /projectnb/dietzelab/DART_analy/histo_$ensemble_member
	cp /projectnb/dietzelab/viskari/histo/$file_name /projectnb/dietzelab/DART_analy/histo_$ensemble_member
   endif

   if(-e /projectnb/dietzelab/DART_analy/histo_$ensemble_member/$file_name) then
   else
	cp /projectnb/dietzelab/viskari/histo/$file_name /projectnb/dietzelab/DART_analy/histo_$ensemble_member
   endif

   # Copies the history file from the histo folder to the temporary directory.
   cp /projectnb/dietzelab/DART_analy/histo_$ensemble_member/$file_name .

   rm -f histo.dat
   rm -f analy.dat
   echo "'/projectnb/dietzelab/DART_analy/histo_"$ensemble_member"/duke'" > histo.dat
   echo "'/projectnb/dietzelab/DART_analy/analy_"$ensemble_member"/duke'" > analy.dat

   # Runs the R scripts which produce the ED2 namelist for the relevant runs and writes the state vector values to the history file.
   R CMD BATCH RH_B.R

   

   # Copies the history file back to the history folder for the ED2 run.
   cp $file_name /projectnb/dietzelab/DART_analy/histo_$ensemble_member/

   #-------------------------------------------------------------------
   # Block 3: advance the model
   #          In this case, we are saving the run-time messages to
   #          a LOCAL file, which makes debugging easier.
   #          integrate_model is hardcoded to expect input in temp_ic 
   #          and it creates temp_ud as output. 
   #          Your model will likely be different.
   #-------------------------------------------------------------------

   set i = `head start_date`
   set j = `head end_date`

   echo $i > date.dat
   echo $j >> date.dat

   R CMD BATCH createInput.R

   ./ed_2.1-opt -s || exit 3

   # (OPTIONAL) Append the run-time messages to the file in the CENTRALDIR
   #cat integrate_model_out_temp >> ../integrate_model_out_temp${ensemble_member}

   #-------------------------------------------------------------------
   # Block 4: Move the updated state vector back to CENTRALDIR
   #          (temp_ud was created by integrate_model and is in the 
   #          right format already.) In general, you must convert your 
   #          model output to a DART ics file with the proper name.
   #-------------------------------------------------------------------

   # Names and copies the history file containing the advanced state vector values
  set end_name = `head end_file.txt`
  cp /projectnb/dietzelab/DART_analy/histo_$ensemble_member/$end_name .

   # Runs the R script which reads the advanced state vector values from the history file.
   R CMD BATCH H_B.R

   cat Routput.dat

   # Writes the advanced state vector values to a binary format for DART.
   ./R2F

   # Moves that advanced state vector out of the temporary folder.
   mv temp_output.dat ../$output_file || exit 4

   @ state_copy++
   @ ensemble_member_line = $ensemble_member_line + 3
   @ input_file_line = $input_file_line + 3
   @ output_file_line = $output_file_line + 3
end

# Change back to original directory and get rid of temporary directory.
# If all goes well, there should be no need to keep this directory.
# If you are debugging, you may want to keep this directory. 

cp -r end_file.txt ..
cp -r end_date ..
cd ..
\rm -rf $temp_dir

# MANDATORY - Remove the control_file to signal completion. If it still
# exists in CENTRALDIR after all the ensemble members have been advanced,
# it means one or more of the advances failed and is an ERROR CONDITION.

\rm -rf $control_file

exit 0

# <next few lines under version control, do not edit>
# $URL: https://proxy.subversion.ucar.edu/DAReS/DART/releases/Kodiak/models/template/shell_scripts/advance_model.csh $
# $Revision: 4945 $
# $Date: 2011-06-02 18:29:30 -0400 (Thu, 02 Jun 2011) $

