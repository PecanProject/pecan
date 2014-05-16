
nvars=("air_pressure" "surface_downwelling_shortwave_flux" "surface_downwelling_longwave_flux" "air_temperature" "specific_humidity" "precipitation_flux" "eastward_wind" "northward_wind" )

sep="."
suffix=".nc"

n=${#nvars[*]} #Number of variable names
  
  ######################
  # Merge files by year
  
  cd /projectnb/cheas/pecan.data/input/NARR_CF
  
  for i in {1979..2013}
  do   
  year="$i"
  
  for (( k=0; k<=$(( $n -1 )); k++ )) # For each variable name
    do    
  file=${nvars[$k]}$sep$year$suffix
  
  if [ -f $file ]; then
  
  mv $file variables/$file
  fi
  
  done
  done
  