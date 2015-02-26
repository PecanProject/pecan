##' Functions for converting spectral data into matrices for use in Bayesian inversion.

library(reshape2)
library(data.table)

##' @name specmatrix
##' @title Observed reflectance matrix
##' @details {
##' Binds requisite spectra from '.csv' files into a matrix useable by Bayesian inversion function.
##' }
##' 
##' @param Species Name of species to be used. Must EXACTLY match species tag in file name.
##' @param wln Starting wavelength (default=400)
##' @param wlx Maximum wavelength (default=2500)
##' @param path Path to '.csv' spectra (default="data/SE_spectra/Reflectance/")
##' @return Observed reflectance matrix usable by Bayesian inversion function.
##' @export
##' @author Alexey Shiklomanov

specmatrix <- function(species,
                       wln=400,
                       wlx=2500,
                       spectype="SE",
                       filter.spec=TRUE
                       ){
        print(sprintf("Loading %s ...", species))
        path <- switch(spectype,
                       SE = "~/Documents/Dropbox/SE_spectra/Reflectance",
                       FFT = "~/Documents/Dropbox/FFT_spectra/NASA_FFT_LC_Refl_Spectra_v4.csv")
        if(spectype=="FFT"){
                ## FFT spectra
                filter.spec <- FALSE
                raw.list <- data.frame(fread(path, header=TRUE))
                refl.list <- subset(raw.list, Species==species)[,-(2:21)]
                refl.mat <- t(as.matrix(refl.list[,-1]))
                colnames(refl.mat) <- refl.list$Spectra
                wavelengths <- rownames(refl.mat)
                refl.mat <- refl.mat[-grep("Wave_3[0-9][0-9]", wavelengths),]
                refl.mat <- as.matrix(refl.mat)
        } else if (spectype == "SE"){
                flist <- list.files(path)
                flist.split <- strsplit(flist, "_")
                fset.inds <- which(sapply(flist.split, "[", 5) == species)
                fset <- flist[fset.inds]
                raw.list <- lapply(fset, function(x) read.csv(paste(path,x,sep=''),
                                                              header=TRUE))
                refl.list <- lapply(raw.list, function(x) {
                                    x[which(x$Wavelength >= wln &
                                            x$Wavelength <= wlx), 3]})
                refl.mat <- do.call(cbind, refl.list)
                refl.mat <- as.matrix(refl.mat)
                colnames(refl.mat) <- fset
        }

        if(filter.spec){
                RowMeans <- rowMeans(refl.mat)
                RowSD <- apply(refl.mat, 1, sd)
                MaxFilter <- RowMeans + 3 * RowSD
                MinFilter <- RowMeans - 3 * RowSD
                specfilter <- function(spec) any(spec < MinFilter | spec > MaxFilter)
                badspec <- apply(refl.mat, 2, specfilter)
                refl.mat.keep <- refl.mat[,!badspec]
                print(dim(refl.mat) - dim(refl.mat.keep))
                refl.mat <- refl.mat.keep
        }
        print("---- Done ----")
        return(refl.mat)
}

##' @name gen.spec.list
##' @title List available species
##' @details {
##' Creates a list of available species tags, for reference or for batch jobs.
##' }
##' 
##' @param path Path to '.csv' spectra (default="data/SE_spectra/Reflectance/")
##' @param out Store in local memory? If TRUE, returns character vector with list. If FALSE, prints to "scripts/species_list.txt".
##' @return If out==TRUE, returns character vector of species tags.
##' @export
##' @author Alexey Shiklomanov

gen.spec.list <- function(spectype="SE",
                          out=FALSE){
        path <- switch(spectype,
                       SE = "~/Documents/Dropbox/SE_spectra/Reflectance/",
                       FFT = "~/Documents/Dropbox/FFT_spectra/NASA_FFT_LC_Refl_Spectra_v4.csv")
        writepath <- switch(spectype,
                            SE = "../data/species_list_SE.txt",
                            FFT = "../data/species_list_FFT.txt")
        if(spectype=="FFT"){
                ## FFT spectra
                raw.list <- data.frame(fread(path, header=TRUE))
                spec.list <- unique(raw.list$Species)
                spec.list <- sprintf("FFT\\.%s", spec.list)
        } else if (spectype == "SE"){
                flist <- list.files(path)
                flist.split <- strsplit(flist, "_")
                spec.list <- unique(sapply(flist.split, "[", 5))
                spec.list <- sprintf("SE\\.%s", spec.list)
        }        
        if(out) {
                return(spec.list)
        } else {
                
                write(spec.list, writepath,
                      ncolumns = 1, append=FALSE)
        }
}
