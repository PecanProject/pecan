### FORTRAN inversion of PROSPECT
system("R CMD SHLIB fortran_inversion/prospect.f90")
dyn.load("fortran_inversion/prospect.so")
call.args <- list("TAU", k = 3.0, x = 0)
z <- do.call(.Fortran, call.args)
