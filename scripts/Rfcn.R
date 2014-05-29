#!/usr/bin/env Rscript

## code to run an arbitrary R function
## intended to be used to run commands on a remote server
## e.g.  ssh <machine> <package> <function> <args>

args <- commandArgs(trailingOnly = TRUE)

## check args
if(length(args)<2){
  print(c("Insufficient args",args))
  exit()
}

## load packages
if(args[1] != "NULL"){
  pkgs = unlist(strsplit(args[1],",",fixed=TRUE))
  for(i in 1:length(pkgs)){
    do.call(require, list(pkgs[i]))
  }
}

## check that function exists
if(exists(args[2])){
  
  ## put function arguments into a list
  fcn.args = list()
  if(length(args)>2){
    for(i in 1:(length(args)-2)){
      fcn.args[[i]] = args[i+2]
    }
  }

  ## call function
  output <- do.call(args[2], fcn.args) 
}