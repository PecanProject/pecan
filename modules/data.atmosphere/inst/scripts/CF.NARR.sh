
vars=("pres.sfc" "dswrf" "dlwrf" "air.2m" "shum.2m" "prate" "uwnd.10m" "vwnd.10m")
svars=("pres" "dswrf" "dlwrf" "air" "shum" "prate" "uwnd" "vwnd")
nvars=("air_pressure" "surface_downwelling_shortwave_flux" "surface_downwelling_longwave_flux" "air_temperature" "specific_humidity" "precipitation_flux" "eastward_wind" "northward_wind" )

inpath  = $1
prefix  = $2
outpath = $3

sep="."
suffix=".nc"

n=${#nvars[*]} #Number of variable names

######################
# Rename Variables

for i in {1979..2013}
do   
    year="$i"

    for (( k=0; k<=$(( $n -1 )); k++ )) # For each variable name
        do    
        
            file = $inpath${vars[$k]}$sep$year$suffix

            newfile = $outpath${nvars[$k]}$sep$year$suffix

            if [ -f $file ] && [ ! -f $newfile ];
                then
                cp $file $newfile
                ncrename -v ${svars[$k]},${nvars[$k]} $newfile
            fi
        done
done

######################
# Merge files by year

cd $outfolder

for i in {1979..2013}
    do   
        year="$i"
        
        j=0

        for (( k=0; k<=$(( $n -1 )); k++ )) # For each variable name
            do    
                file=${nvars[$k]}$sep$year$suffix
                
                if [ -f $file ]; then
                    let j++

                    if [ $j == 1 ]; then
                        cp $file $prefix$year$suffix 

                        ncks -O --fl_fmt=netcdf4 $prefix$year$suffix $prefix$year$suffix   # netCDF4
                        ncpdq -O -U $prefix$year$suffix $prefix$year$suffix 

                    else 

                        ncks -O --fl_fmt=netcdf4 $file $file  # netCDF4
                        ncpdq -O -U $file $file

                        
                        ncks -A $file $prefix$year$suffix 

                    fi
                    mv $file variables/$file
                fi

        done
done
