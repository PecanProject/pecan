# Makefile for radiative transfer models
# Author: Alexey Shiklomanov, Dept of Earth and Environment / Boston University
# Email: ashiklom@bu.edu

# Compilers
FC = gfortran -ffree-form
FC_fixed = gfortran -ffixed-form

# Optimization
OPT = -O2

# Folder paths
f_prospect = prospect/
f_sail = sail/
f_ps = prosail/
f_ds = dataSpec/

# Module shorthand
mod_f = $(f_ds)prosail.dataSpec_
mod_m = mod_dataspec_

# RTM shared libraries
PROSPECT = prospect.so
SAIL = sail.so
PROSAIL = prosail.so

# RTM dependencies
PROSPECT_depends = $(f_prospect)*.f90 $(mod_m)prospect5b.o $(mod_m)refractive.o
SAIL_depends = $(PROSPECT) $(f_sail)*.f90 $(mod_m)soil.o
PROSAIL_depends = $(PROSPECT_depends) $(SAIL_depends) $(f_ps)prosail.main.f90

$(PROSAIL) : $(PROSAIL_depends)
	$(FC) -g -shared $(PROSAIL_depends) -o $(PROSAIL) -fPIC 

$(PROSPECT) : $(PROSPECT_depends)
	$(FC) -g -shared $(PROSPECT_depends) -o $(PROSPECT) -fPIC

$(SAIL) : $(SAIL_depends)
	$(FC) -g -shared $(SAIL_depends) -o $(SAIL) -fPIC

$(mod_m)prospect5b.o $(mod_m)refractive.o $(mod_m)soil.o : $(mod_f)wavelength.f90
	$(FC) -c $(mod_f)wavelength.f90 -o $(mod_m)wavelength.o
	$(FC) -c $(mod_f)prospect5b.f90 -o $(mod_m)prospect5b.o 
	$(FC) -c $(mod_f)refractive.f90 -o $(mod_m)refractive.o 
	$(FC) -c $(mod_f)soil.f90 -o $(mod_m)soil.o 

clean :
	rm *.o 
