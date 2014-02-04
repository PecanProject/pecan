
# The rhdf5 package will contain a complete installation of the HDF5 C library.
# No further downloads from HDF5 are necessary. 
# This file contains code for bioconductor administrators.

replacestr <- function(file, pat1, pat2, newtext) {
  L = readLines(file)
  I = grep(pat1,L,fixed=TRUE)
  n=0
  while(length(I) > 0) {
    n=n+1
    I = I[1]
    J=I+grep(pat2,L[I:length(L)],fixed=TRUE)[1]-1
    cat("\n#######################\nIn file ",file," REPLACE\n")
    cat("lines ",I," to ",J,"\n")
    cat(paste(L[I:J],collapse="\n"))
    cat("\nWITH\n",newtext,"\n")
    L = c(L[1:(I-1)],newtext,L[(J+1):(length(L))])
    offset = (I+length(newtext))
    I = grep(pat1,L[offset:length(L)],fixed=TRUE)
    if (length(I) > 0) {
      I = I + offset - 1
    }
  }
  writeLines(L, file)
  invisible(n)
}

sed <- function(file, replace, with) {
  L = readLines(file)
  K = sub(replace, with, L, fixed=TRUE)
  I <- which(L != K)
  n <- 0
  for (i in I) {
    n <- n + 1
    cat("\n#######################\nIn file",file, "REPLACE line",i,"\n")
    cat(L[i],"\n")
    cat("WITH\n")
    cat(K[i],"\n")
  }
  writeLines(K, file)
  invisible(n)
}

n = rep(0,12)
n[1] = replacestr("configure.in", "AC_CONFIG_FILES", "])", c("AC_CONFIG_FILES([src/libhdf5.settings"," src/Makefile])"))
n[2] = replacestr("configure", "ac_config_files=\"", "ac_config_files=\"", "ac_config_files=\"$ac_config_files src/libhdf5.settings Makefile src/Makefile\"")
n[3] = sed("configure", "-install_name \\$rpath/\\$soname", "-install_name \\$soname")
n[4] = sed("configure", "library_names_spec='${libname}${release}${major}$shared_ext", "library_names_spec='${libname}${release}$shared_ext")
n[5] = sed("configure", "soname_spec='${libname}${release}${major}$shared_ext'", "soname_spec='${libname}${release}$shared_ext'")
n[6] = replacestr("configure", "test .$module", "test .$module", "  shrext_cmds='.so'")
n[7] = replacestr("Makefile.in", "SUBDIRS = src test $(TESTPARALLEL_DIR) tools", "perform", "SUBDIRS = src")
n[8] = replacestr("Makefile.in", "DIST_SUBDIRS = src test testpar", "examples", "DIST_SUBDIRS = src")
n[9] = replacestr("src/Makefile.in", "lib_LTLIBRARIES = libhdf5.la", "lib_LTLIBRARIES = libhdf5.la", "lib_LTLIBRARIES = libhdf5ForBioC.la")
n[10] = replacestr("src/Makefile.am", "lib_LTLIBRARIES=libhdf5.la", "lib_LTLIBRARIES=libhdf5.la", "lib_LTLIBRARIES = libhdf5ForBioC.la")
n[11] = sed("src/Makefile.in", "libhdf5.la:", "libhdf5ForBioC.la:")
n[12] = sed("configure", "$H5_CFLAGS $", "$H5_CFLAGS -w $")

N <- c(1,1,8,3,3,3,1,1,1,1,1,3)

M = data.frame(replacement = n,intended = N, correct=n==N)
row.names(M) = c("AC_CONFIG_FILES","as_config_files", "install_name", "library_names_spec", "soname_spec", "dylib", "SUBDIRS", "DIST_SUBDIRS", "LTLIBRARIES_1", "LTLIBRARIES_2", "libhdf5.la:", "-Wconversion")

cat("Summary of replacements\n")
print(M)

