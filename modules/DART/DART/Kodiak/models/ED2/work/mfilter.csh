#!/bin/csh

@ i = 2002

while($i <= 2005)
    rm -f sim_year
    echo $i > sim_year
    cp obs_seq.out_f$i obs_seq.out

    mpirun -np 4 filter

    cp obs_seq.final obs_seq.final_f$i
    cp Posterior_Diag.nc Posterior_Diag.nc_f$i
    cp Prior_Diag.nc Prior_Diag.nc_f$i

    @ i += 1
end
    
    cp obs_seq.out_2002 obs_seq.out
    echo 2002 > sim_year
