#
# This (currently incomplete) R script is intended to extract equations
# from the BIOPAK equation library, parse them into executable R
# functions, and apply them appropriately to the individual tree records
# in the "FIA_indivtrees.csv" file.
#
# The code is mostly working (in prototype fashion), but some issues
# are:
# - Matching up of species codes in the FIA data and in the equation
#   library is imperfect. Several issues still to work out here.
# - In cases where there are more than one equation for the same quantity
#   for a particular tree species, the code currently just takes the
#   mean of the different outputs
# - Parts of the code might break if you change things like column
#   headers, etc. This could be handled easily enough with function
#   arguments, but for the most part isn't yet.
# - Speed problems: The cast/melt statement that converts the final list
#   into a table is slooooooowwwww on the full 230k+ record FIA dataset.
#   Note that everything before that happens in <10 minutes, which is
#   not great but probably fast enough. Almost certainly can do some
#   things to make the code faster if necessary. Also note that
#   performance would probably get worse if you wanted more advanced
#   logic about which equation to use in particular cases.
#
# Note: the "reshape" package is currently required, although this may
# change
#
# Author: Jim Regetz, with David LeBauer (Functional Ecology DGS)
# Created on 28-Mar-2008
# NCEAS

library(reshape)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function definitions

# Formula generator. Takes a character string of a BIOPAK equation, and
# converts the right hand side of the equation into an executable R
# expression. This expression is returned as a function. Currently only
# works if the RHS of the equation only has numeric constants, standard
# math operators, and the variable "DBH". If the equation give the LHS
# as ln(varname), the result is exponentiated.
getEquation <- function(equation) {

 equation <- as.character(equation)
 ln <- log

 eq.split <- strsplit(equation, " = ")[[1]]

 LHS <- eq.split[1]
 responseName <- gsub("^ln\\(([[:alpha:]]*)\\)", "\\1", LHS)

 RHS <- eq.split[2]

 function(DBH) {
   ans <- eval(parse(text=RHS))
   if (substr(LHS, 1, 2)=="ln") ans <- exp(ans)
   names(ans) <- responseName
   return(ans)
 }

}

# Function to apply the equations to each tree (row) of an input
# dataframe that contains (at least) a "spp" column (4-letter species
# codes used in the BIOPAK equation library) and a "dbh" column
# (numeric dbh values)
runEquations <- function(dat, sp.col, dbh.col) {
 mapply(function(id, spp, dbh) sapply(equation[[spp]],
   function(calc) calc(dbh)), id=seq_len(nrow(dat)),
   spp=as.character(dat[,sp.col]), dbh=dat[,dbh.col], SIMPLIFY=FALSE)
}

# Function to take the mean of calculated variables in cases where more
# than one equation is used for the same outcome variable
getMeans <- function(listOfVals) {
 lapply(listOfVals, function(x) tapply(x, names(x), mean))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read in some input data
eqlib <- read.csv("biolib10.csv")
fiadb <- read.csv("FIA_individtrees.csv", row.names=1)
spcodes <- read.csv("spcodes.csv", fill=TRUE)
spcodes$autocode <- toupper(paste(substr(spcodes$Genus, 1, 2),
 substr(spcodes$Species, 1, 2), sep=""))

# --- TEMPORARY --- #
# Add a new column for 4-letter species code by automatically combining
# the first 2 letters of the genus name with the first two letters with
# the species name (both as given in Table 4 of [whatever it is].
fiadb <- merge(fiadb, spcodes[c("FIA.ID", "autocode")], by.x="FIA.SPCD",
 by.y="FIA.ID", sort=FALSE, all.x=TRUE)
# ----------------- #

# Reduce equation library to the relevant subset of equations
responses <- c("BAT", "BBD", "BBL", "BBT", "BFN", "BFT", "BRT", "BSB",
 "BST", "BSW", "PFT")
eqlib.sub <- subset(eqlib, LIFEFORM=="T" & PA1_CODE=="DBH" & is.na(PA2_CODE) &
 BIO_COMP %in% responses)
eqlib.sub$SPP_CODE <- as.character(eqlib.sub$SPP_CODE)
eqlib.sub$BPK_EQFM <- as.character(eqlib.sub$BPK_EQFM)

# Parse the equations for each species into executable R functions
# stored in a list
equation <- sapply(unique(eqlib.sub$SPP_CODE), function(sp)
 lapply(eqlib.sub$BPK_EQFM[eqlib.sub$SPP_CODE==sp], getEquation))

# Generate fake data
#numInd <- 1000
#testdata <- data.frame(
#  id=seq_len(numInd),
#  spp=sample(eqlib.sub$SPP_CODE, numInd, replace=TRUE),
#  DBH=runif(numInd, 1, 100)
#)

# Run the equations on each individual. Then in cases where there are
# multiple estimated values for a given variable (b/c multiple equations
# in the equation library for this species), take the mean
allVals <- runEquations(fiadb, sp.col="autocode", dbh.col="FIA.DIA")

# TEMPORARY HACK? #
# This deals with null elements that arise in the allVals when running
# the code on the FIA data. My UNCONFIRMED suspicion is that these nulls
# arise when species codes in the data are not matched by any species
# codes in the equations list. Obviously this should be handled in the
# "runEquations" functions, but isn't yet.
allVals <- allVals[sapply(allVals, function(x) length(x)>0)]
#-----------------#

meanVals <- getMeans(allVals) # Turn the ragged list above into a dataframe, and clean up
meanValsDF <- cast(melt(meanVals), L1 ~ indicies)

#---THIS LAST STEP TAKES A LONG TIME WITH THE FULL FIA DATA---#:w
# Merge calculated values with the original FIA table
# next line gets error "Error in as.data.frame(y) : object "meanValsDF" not found" :

# result <- merge(fiadb, meanValsDF, by.x=0, by.y="L1", sort=FALSE)

