# Makefile for radiative transfer models
# Author: Alexey Shiklomanov, Dept of Earth and Environment / Boston University
# Email: ashiklom@bu.edu

# Fortran compiler
FC = gfortran -fPIC -g -O3
CC = gcc -fPIC -g
Rin = -I/usr/share/R/include
RC = R CMD SHLIB 

# ------- Dependency trees --------
# Statistics
stats_core = random.o mod_statistics.o
stats_full = $(stats_core) samplers.o inversion.o prior.o
select_full = mod_rtm.o mod_selectmodel.o mod_combine.o

# RTM dependencies
PROSPECT_depends = RTM/prospect/*.f90 mod_dataspec_prospect5b.o mod_dataspec_refractive.o
SAIL_depends = $(PROSPECT_depends) RTM/sail/*.f90 mod_dataspec_soil.o
PROSAIL_depends = $(SAIL_depends) RTM/prosail/prosail.main.f90
package_depends = $(PROSAIL_depends) $(stats_full) $(select_full)

all : $(package_depends)
	$(RC) $(package_depends) -o PEcAnRTM.so

# Statistics
samplers.o : statistics/samplers.f90 $(stats_core) mod_types.o mod_dataspec_wavelength.o prior.o
	$(FC) -c statistics/samplers.f90 -o samplers.o 

inversion.o : statistics/inversion.f90 samplers.o mod_selectmodel.o
	$(FC) -c statistics/inversion.f90 -o inversion.o

random.o : statistics/random.f90
	$(FC) -c statistics/random.f90 -o random.o

mod_statistics.o : statistics/mod_statistics.f90 mod_types.o
	$(FC) -c statistics/mod_statistics.f90 -o mod_statistics.o

prior.o : statistics/prior.f90
	$(FC) -c statistics/prior.f90 -o prior.o

mod_selectmodel.o : RTM/mod_selectmodel.f90 mod_types.o mod_rtm.o
	$(FC) -c RTM/mod_selectmodel.f90 -o mod_selectmodel.o

mod_rtm.o : RTM/mod_rtm.f90 mod_types.o mod_dataspec_wavelength.o mod_combine.o
	$(FC) -c RTM/mod_rtm.f90 -o mod_rtm.o

mod_combine.o : RTM/mod_combine.f90 mod_types.o
	$(FC) -c RTM/mod_combine.f90 -o mod_combine.o

mod_types.o : statistics/mod_types.f90
	$(FC) -c statistics/mod_types.f90 -o mod_types.o

mod_dataspec_prospect5b.o mod_dataspec_refractive.o mod_dataspec_soil.o mod_dataspec_wavelength.o :
	$(FC) -c RTM/dataSpec/prosail.dataSpec_wavelength.f90 -o mod_dataspec_wavelength.o
	$(FC) -c RTM/dataSpec/prosail.dataSpec_prospect5b.f90 -o mod_dataspec_prospect5b.o 
	$(FC) -c RTM/dataSpec/prosail.dataSpec_refractive.f90 -o mod_dataspec_refractive.o 
	$(FC) -c RTM/dataSpec/prosail.dataSpec_soil.f90 -o mod_dataspec_soil.o 

clean :
	rm *.o 

purge :
	rm *.o *.so *.mod
