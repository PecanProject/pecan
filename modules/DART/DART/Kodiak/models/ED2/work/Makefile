# Makefile created by mkmf $Id: mkmf 4945 2011-06-02 22:29:30Z thoar $ 

include ../../../mkmf/mkmf.template

CPPDEFS = -Duse_netCDF

.DEFAULT:
	-touch $@
all: wakeup_filter
time_manager_mod.o: ../../../time_manager/time_manager_mod.f90 types_mod.o utilities_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	../../../time_manager/time_manager_mod.f90
mpi_utilities_mod.o: ../../../mpi_utilities/mpi_utilities_mod.f90 types_mod.o utilities_mod.o time_manager_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	../../../mpi_utilities/mpi_utilities_mod.f90
wakeup_filter.o: ../../../filter/wakeup_filter.f90 mpi_utilities_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	../../../filter/wakeup_filter.f90
utilities_mod.o: ../../../utilities/utilities_mod.f90 types_mod.o
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	../../../utilities/utilities_mod.f90
types_mod.o: ../../../common/types_mod.f90
	$(MPIFC) $(FFLAGS) $(OTHERFLAGS) -c	../../../common/types_mod.f90
./utilities_mod.f90: ../../../utilities/utilities_mod.f90
	cp ../../../utilities/utilities_mod.f90 .
./types_mod.f90: ../../../common/types_mod.f90
	cp ../../../common/types_mod.f90 .
./wakeup_filter.f90: ../../../filter/wakeup_filter.f90
	cp ../../../filter/wakeup_filter.f90 .
./mpi_utilities_mod.f90: ../../../mpi_utilities/mpi_utilities_mod.f90
	cp ../../../mpi_utilities/mpi_utilities_mod.f90 .
./time_manager_mod.f90: ../../../time_manager/time_manager_mod.f90
	cp ../../../time_manager/time_manager_mod.f90 .
SRC = ../../../time_manager/time_manager_mod.f90 ../../../mpi_utilities/mpi_utilities_mod.f90 ../../../filter/wakeup_filter.f90 ../../../utilities/utilities_mod.f90 ../../../common/types_mod.f90
OBJ = time_manager_mod.o mpi_utilities_mod.o wakeup_filter.o utilities_mod.o types_mod.o
OFF = ../../../utilities/utilities_mod.f90 ../../../common/types_mod.f90 ../../../filter/wakeup_filter.f90 ../../../mpi_utilities/mpi_utilities_mod.f90 ../../../time_manager/time_manager_mod.f90
clean: neat
	-rm -f .cppdefs $(OBJ) wakeup_filter
neat:
	-rm -f $(TMPFILES)
localize: $(OFF)
	cp $(OFF) .
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
wakeup_filter: $(OBJ)
	$(MPILD) $(OBJ) -o wakeup_filter  $(LDFLAGS)
