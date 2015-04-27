# Makefile for radiative transfer models
# Author: Alexey Shiklomanov, Dept of Earth and Environment / Boston University
# Email: ashiklom@bu.edu

# Fortran compiler
FC = gcc

# RTM dependencies
PROSPECT_depends = RTM/prospect/*.f90 mod_dataspec_prospect5b.o mod_dataspec_refractive.o
SAIL_depends = $(PROSPECT_depends) RTM/sail/*.f90 mod_dataspec_soil.o
PROSAIL_depends = $(SAIL_depends) RTM/prosail/prosail.main.f90
package_depends = $(PROSAIL_depends)

PEcAnRTM.so : $(package_depends)
	$(FC) -g -shared $(package_depends) -o PEcAnRTM.so -fPIC

mod_dataspec_prospect5b.o mod_dataspec_refractive.o mod_dataspec_soil.o mod_dataspec_wavelength.o :
	$(FC) -c RTM/dataSpec/prosail.dataSpec_wavelength.f90 -o mod_dataspec_wavelength.o
	$(FC) -c RTM/dataSpec/prosail.dataSpec_prospect5b.f90 -o mod_dataspec_prospect5b.o 
	$(FC) -c RTM/dataSpec/prosail.dataSpec_refractive.f90 -o mod_dataspec_refractive.o 
	$(FC) -c RTM/dataSpec/prosail.dataSpec_soil.f90 -o mod_dataspec_soil.o 

clean :
	rm *.o 
