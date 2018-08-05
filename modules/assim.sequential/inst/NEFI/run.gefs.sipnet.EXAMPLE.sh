# This script first runs a program which sets up the xml file for a current
# NOAA_GEFS PEcAn run, then runs PEcAn with that file.
# @author Luke Dramko

# REPLACE < username > WITH YOUR USERNAME
# If running from a CRON job, these paths MUST be absolute paths.  This is because CRON assumes that the directory it is in is the working directory.
xmlfile="./gefs.sipnet.source.xml" #Path to, and name of, the base xml file.
workflow_path="./../../../../scripts/"                     #Path to workflow.R (in pecan/web for the standard version or pecan/scripts for the custom version).
output_path="/fs/data3/< username >/output/"               #Path to the directory where all PEcAn output is put.
xmlscript="./generate.gefs.xml.R"                          #Path to, and name of, the script that modifies the xml.
# Could also be just workflow.R in pecan/web
workflow_name="workflow.wcr.assim.R"                       #Name of the workflow.R version

# Generates the xml file based on a given input file.  Overwrites the
# input file.
Rscript $xmlscript $xmlfile $1 &> /dev/null
if [ $? -eq 11 ];
then
  echo "xml file not found."
elif [ $? -eq 12 ]
then
  echo "Database connection failed."
else
  # Find the most recently created output directory.  This system is kind of hack-y, and is messed up if anything after
  # "PEcAn", alphabetically, is put in the directory.  Fortunately, that is unlikely to happen.
  output_dir=$(ls $output_path | sort -V | tail -n 1)
  # Runs the PEcAn workflow.
  Rscript ${workflow_path}${workflow_name} $xmlfile &> ${output_path}/${output_dir}/workflow.log.txt
  echo "Workflow completed."
fi
