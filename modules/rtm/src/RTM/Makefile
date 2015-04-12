# Makefile for radiative transfer models
# Author: Alexey Shiklomanov, Dept of Earth and Environment / Boston University
# Email: ashiklom@bu.edu

# Compilers
FC = gfortran -ffree-form
FC_fixed = gfortran -ffixed-form

# Optimization
OPT = -O2

# Folder paths
bin = bin/
f_prospect = prospect/
f_sail = sail/
f_ds = dataSpec/

# Module shorthand
mod_f = $(f_ds)prosail.dataSpec_
mod_m = $(bin)mod_dataspec_

# RTM shared libraries
PROSPECT = $(bin)prospect.so
SAIL = $(bin)sail.so

# RTM dependencies
PROSPECT_depends = $(f_prospect)*.f $(mod_m)prospect5b.o $(mod_m)refractive.o
SAIL_depends = $(PROSPECT) $(f_sail)*.f $(mod_m)soil.o
RTM_depends = $(PROSPECT) $(SAIL)

$(bin)RTM.so : $(RTM_depends)
	$(FC) -shared $(SAIL) $(PROSPECT) -o prosail.so -fPIC

$(PROSPECT) : $(PROSPECT_depends)
	$(FC) -shared $(PROSPECT_depends) -o $(PROSPECT) -I$(bin) -fPIC

$(SAIL) : $(SAIL_depends)
	$(FC) -shared $(SAIL_depends) -o $(SAIL) -I$(bin) -fPIC

$(mod_m)prospect5b.o $(mod_m)refractive.o $(mod_m)soil.o : $(mod_f)wavelength.f
	$(FC) -c $(mod_f)wavelength.f -J$(bin) -o $(mod_m)wavelength.o
	$(FC) -c $(mod_f)prospect5b.f -I$(bin) -J$(bin) -o $(mod_m)prospect5b.o 
	$(FC) -c $(mod_f)refractive.f -I$(bin) -J$(bin) -o $(mod_m)refractive.o 
	$(FC) -c $(mod_f)soil.f -I$(bin) -J$(bin) -o $(mod_m)soil.o 

clean :
	rm -r bin
