#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------#
# Small, miscellaneous functions for use throughout PECAn
#--------------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------------#
##' return MstMIP variable as ncvar
##'
##' returns a MstMIP variable as a ncvar based on name and other parameters
##' passed in.
##'
##' @title MstMIP variable
##' @export
##' @param name of variable
##' @param lat latitude if dimension requests it
##' @param lon longitude if dimension requests it
##' @param time time if dimension requests it
##' @param nsoil nsoil if dimension requests it
##' @param silent logical: suppress log messages about missing variables?
##' @return ncvar based on MstMIP definition
##' @author Rob Kooper
mstmipvar <- function(name, lat = NULL, lon = NULL, time = NULL, nsoil = NULL, silent = FALSE) {
  nc_var <- PEcAn.utils::standard_vars[PEcAn.utils::standard_vars$Variable.Name == name, ]
  
  if (nrow(nc_var) == 0) {
    if (!silent) {
      PEcAn.logger::logger.info("Don't know about variable", name, " in standard_vars in PEcAn.utils")
    }
    if (is.null(time)) {
      time <-
        ncdf4::ncdim_def(
          name = "time",
          units = "days since 1900-01-01 00:00:00",
          vals = 1:365,
          calendar = "standard",
          unlim = TRUE
        )
    }
    return(ncdf4::ncvar_def(name, "", list(time), -999, name))
  }
  
  var_dims <- nc_var[paste0("dim", 1:4)]
  pos_dims <- c("lon", "lat", "time", "nsoil")
  
  #check for missing dimensions
  no_dims <- pos_dims[!var_dims %in% pos_dims]
  if (!silent) {
    if (length(no_dims) > 0) {
      PEcAn.logger::logger.info("Don't know dimension(s)", no_dims, "for variable", name)
    }
  }
  
  #replace dim names with values
  var_dims <- var_dims[var_dims %in% pos_dims]
  var_dims <- lapply(var_dims, function(x) eval(str2lang(x))) #converts character values in `var_dims` to corresponding R objects
  dims <- var_dims[!sapply(var_dims, is.null)] #get rid of NULL elements
  
  ncvar <- ncdf4::ncvar_def(name, as.character(nc_var$Units), dims, -999)
  if (nc_var$Long.name != "na") {
    ncvar$longname <- as.character(nc_var$Long.name)
  }
  return(ncvar)
} 


#--------------------------------------------------------------------------------------------------#
##' left padded by zeros up to a given number of digits.
##'
##' returns a string representing a given number
##' @title Left Pad Zeros
##' @export
##' @param num number to be padded (integer)
##' @param digits number of digits to add
##' @return num with zeros to the left
##' @export
##' @author Carl Davidson
left.pad.zeros <- function(num, digits = 5) {
  format_string <- paste0("%", sprintf("0%.0f.0f", digits))
  return(sprintf(format_string, num))
} # left.pad.zeros


##' Truncates vector at 0
##' @name zero.truncate
##' @title Zero Truncate
##' @param y numeric vector
##' @return numeric vector with all values less than 0 set to 0
##' @export
##' @author unknown
zero.truncate <- function(y) {
  y[y < 0 | is.na(y)] <- 0
  return(y)
} # zero.truncate


#--------------------------------------------------------------------------------------------------#
##' R implementation of rsync
##'
##' rsync is a file copying tool in bash
##' @title rsync
##' @param args rsync arguments (see man rsync)
##' @param from source
##' @param to destination
##' @param pattern file pattern to be matched
##' @return nothing, transfers files as a side effect
##' @export
##' @author David LeBauer
##' @author Shawn Serbin
rsync <- function(args, from, to, pattern = "") {
  PEcAn.logger::logger.warn("NEED TO USE TUNNEL")
  system(paste0("rsync", " ", args, " ", from, pattern, " ", to), intern = TRUE)
} # rsync


#--------------------------------------------------------------------------------------------------#
##' R implementation of SSH
##'
##' @title SSH
##' @param host (character) machine to connect to
##' @param ... Commands to execute. Will be passed as a single quoted string
##' @param args futher arguments
##' @export
ssh <- function(host, ..., args = "") {
  PEcAn.logger::logger.warn("NEED TO USE TUNNEL")
  if (host == "localhost") {
    command <- paste(..., args, sep = "")
  } else {
    command <- paste("ssh -T ", host, " \"", ..., "\" ", args, sep = "")
  }
  system(command)
} # ssh


#--------------------------------------------------------------------------------------------------#
##' Convert vector to comma delimited string
##'
##' vecpaste, turns vector into comma delimited string fit for SQL statements.
##' @title vecpaste
##' @param x vector
##' @return comma delimited string
##' @export
vecpaste <- function(x) paste(paste0("'", x, "'"), collapse = ",")


#--------------------------------------------------------------------------------------------------#
##' returns an id representing a model run
##'
##' Provides a consistent method of naming runs; for use in model input files and indices
##' @title Get Run ID
##' @param run.type character, can be any character; currently 'SA' is used for sensitivity analysis, 'ENS' for ensemble run.
##' @param index unique index for different runs, e.g. integer counting members of an
##' ensemble or a quantile used to which a trait has been perturbed for sensitivity analysis
##' @param trait name of trait being sampled (for sensitivity analysis)
##' @param pft.name name of PFT (value from pfts.names field in database)
##' @param site.id optional site id .This is could be necessary for multisite write=false ensembles.
##' @return id representing a model run
##' @export
##' @examples
##' get.run.id('ENS', left.pad.zeros(1, 5))
##' get.run.id('SA', round(qnorm(-3),3), trait = 'Vcmax')
##' @author Carl Davidson, David LeBauer
get.run.id <- function(run.type, index, trait = NULL, pft.name = NULL, site.id=NULL) {
  result <- paste(c(run.type, pft.name, trait, index, site.id), collapse = "-")
  return(result)
} # get.run.id

#--------------------------------------------------------------------------------------------------#
##' Zero bounded density using log density transform
##'
##' Provides a zero bounded density estimate of a parameter.
##' Kernel Density Estimation used by the \code{\link[stats]{density}} function will cause problems
##' at the left hand end because it will put some weight on negative values.
##' One useful approach is to transform to logs, estimate the density using KDE, and then transform back.
##' @title Zero Bounded Density
##' @param x data, as a numeric vector
##' @param bw The smoothing bandwidth to be used. See 'bw.nrd'
##' @param n number of points to use in kernel density estimate. See \code{\link[stats]{density}}
##' @return data frame with back-transformed log density estimate
##' @author \href{https://stats.stackexchange.com/q/6588/2750}{Rob Hyndman}
##' @references M. P. Wand, J. S. Marron and D. Ruppert, 1991. Transformations in Density Estimation. Journal of the American Statistical Association. 86(414):343-353 \url{http://www.jstor.org/stable/2290569}
##' @export
zero.bounded.density <- function(x, bw = "SJ", n = 1001) {
  y     <- log(x)
  g     <- stats::density(y, bw = bw, n = n)
  xgrid <- exp(g$x)
  g$y   <- c(0, g$y / xgrid)
  g$x   <- c(0, xgrid)
  return(g)
} # zero.bounded.density


#--------------------------------------------------------------------------------------------------#
##' Summarize results of replicate observations in trait data query
##'
##' @title Summarize Results
##' @param result dataframe with results of trait data query
##' @return result with replicate observations summarized
##' @export summarize.result
##' @usage summarize.result(result)
##' @importFrom rlang .data
##' @importFrom magrittr %>%
##' @author David LeBauer, Alexey Shiklomanov
summarize.result <- function(result) {
  ans1 <- result %>%
    dplyr::filter(.data$n == 1) %>%
    dplyr::group_by(.data$citation_id, .data$site_id, .data$trt_id,
                    .data$control, .data$greenhouse, .data$date, .data$time,
                    .data$cultivar_id, .data$specie_id, .data$name, .data$treatment_id) %>%
    dplyr::summarize( # stat must be computed first, before n and mean
      statname = dplyr::if_else(length(.data$n) == 1, "none", "SE"),
      stat = stats::sd(.data$mean) / sqrt(length(.data$n)),
      n = length(.data$n),
      mean = mean(mean)
    ) %>%
    dplyr::ungroup()
  ans2 <- result %>%
    dplyr::filter(.data$n != 1) %>%
    # ANS: Silence factor to character conversion warning
    dplyr::mutate(statname = as.character(.data$statname))
  if (nrow(ans2) > 0) {
    dplyr::bind_rows(ans1, ans2)
  } else {
    return(ans1)
  }
} # summarize.result


#--------------------------------------------------------------------------------------------------#
##' Further summarizes output from summary.mcmc
##'
##' @title Get stats for parameters in MCMC output
##' @param mcmc.summary probably produced by \code{\link[coda]{summary.mcmc}}
##' @param sample.size passed as 'n' in returned list
##' @return list with summary statistics for parameters in an MCMC chain
##' @author David LeBauer
get.stats.mcmc <- function(mcmc.summary, sample.size) {
  a <- list(n = sample.size)
  for (parm in c("beta.o", "sd.y", "sd.site", "sd.trt", "beta.ghs[2]")) {
    parm.name <- ifelse(parm == "beta.ghs[2]", "beta.ghs", parm)
    if (parm %in% rownames(mcmc.summary$statistics)) {
      a[[parm.name]] <- get.parameter.stat(mcmc.summary, parameter = parm)
    } else {
      a[[parm.name]] <- NA
    }
  }
  return(unlist(a))
} # get.stats.mcmc


#--------------------------------------------------------------------------------------------------#
##' A helper function for building a LaTex table.
##'
##' Used by \code{\link{get.parameter.stat}}.
##' @title Paste Stats
##' @name paste.stats
##' @param median 50-percent quantile
##' @param lcl lower confidence limit
##' @param ucl upper confidence limit
##' @param n significant digits for printing. Passed to \code{\link{tabnum}}
##' @export
##' @author David LeBauer
##' @examples
##' paste.stats(3.333333, 5.00001, 6.22222, n = 3)
##' # [1] "$3.33(5,6.22)$"
paste.stats <- function(median, lcl, ucl, n = 2) {
  paste0("$", tabnum(median, n),
         "(", tabnum(lcl, n), ",", tabnum(ucl, n), ")",
         "$")
} # paste.stats


#--------------------------------------------------------------------------------------------------#
##' Gets statistics for LaTeX - formatted table
##'
##' @title Get Parameter Statistics
##' @param mcmc.summary probably produced by \code{\link[coda]{summary.mcmc}}
##' @param parameter name of parameter to extract, as character
##' @return table with parameter statistics
##' @author David LeBauer
##' @export
##' @examples
##' \dontrun{get.parameter.stat(mcmc.summaries[[1]], 'beta.o')}
get.parameter.stat <- function(mcmc.summary, parameter) {
  paste.stats(median = mcmc.summary$quantiles[parameter, "50%"],
              lcl = mcmc.summary$quantiles[parameter, c("2.5%")],
              ucl = mcmc.summary$quantiles[parameter, c("97.5%")],
              n = 2)
} # get.parameter.stat
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' Calculate mean, variance statistics, and CI from a known distribution
##'
##' @title Probability Distirbution Function Statistics
##' @param distn name of distribution used by R (beta, f, gamma, lnorm, norm, weibull)
##' @param A first parameter
##' @param B second parameter
##' @return list with mean, variance, and 95 CI
##' @author David LeBauer
## in future, perhaps create S3 functions: get.stats.pdf <- pdf.stats
pdf.stats <- function(distn, A, B) {
  distn <- as.character(distn)
  mean <- switch(distn,
                 gamma = A/B,
                 lnorm = exp(A + 1/2 * B^2),
                 beta = A/(A + B),
                 weibull = B * gamma(1 + 1/A),
                 norm = A,
                 f = ifelse(B > 2,
                            B/(B - 2),
                            mean(stats::rf(10000, A, B))))
  var <- switch(distn,
                gamma = A/B^2,
                lnorm = exp(2 * A + B ^ 2) * (exp(B ^ 2) - 1),
                beta = A * B/((A + B) ^ 2 * (A + B + 1)),
                weibull = B ^ 2 * (gamma(1 + 2 / A) -
                                     gamma(1 + 1 / A) ^ 2),
                norm = B ^ 2,
                f = ifelse(B > 4,
                           2 * B^2 * (A + B - 2) / (A * (B - 2) ^ 2 * (B - 4)),
                           stats::var(stats::rf(1e+05, A, B))))
  qci <- get(paste0("q", distn))
  ci <- qci(c(0.025, 0.975), A, B)
  lcl <- ci[1]
  ucl <- ci[2]
  out <- unlist(list(mean = mean, var = var, lcl = lcl, ucl = ucl))
  return(out)
} # pdf.stats
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' Dictionary of terms used to identify traits in ed, filenames, and figures
##'
##' @return a dataframe with id, the name used by ED and PEcAn database for a parameter; fileid, an abbreviated
##'     name used for files; figid, the parameter name written out as best known in english for figures
##'     and tables.
##'
##' @param traits a vector of trait names, if traits = NULL, all of the traits will be returned.
##' @export
##' @examples
##' # convert parameter name to a string appropriate for end-use plotting
##' \dontrun{
##' trait.lookup('growth_resp_factor')
##' trait.lookup('growth_resp_factor')$figid
##'
##' # get a list of all traits and units in dictionary
##' trait.lookup()[,c('figid', 'units')]
##' }
trait.lookup <- function(traits = NULL) {
  if (is.null(traits)) {
    return(PEcAn.utils::trait.dictionary)
  }
  PEcAn.utils::trait.dictionary[match(traits, PEcAn.utils::trait.dictionary$id), ]
} # trait.lookup


#--------------------------------------------------------------------------------------------------#
##' Convert number to n significant digits
##'
##' @title Table numbers
##' @param x numeric value or vector
##' @param n number of significant figures
##' @export
##' @author David LeBauer
##' @return x rounded to n significant figures
##' @examples
##' tabnum(1.2345)
##' tabnum(1.2345, n = 4)
tabnum <- function(x, n = 3) {
  ans <- as.numeric(signif(x, n))
  names(ans) <- names(x)
  return(ans)
} # tabnum
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' Scale temperature dependent trait from measurement temperature to reference temperature
##'
##' @title Arrhenius scaling
##' @param observed.value observed value of temperature dependent trait, e.g. Vcmax, root respiration rate
##' @param old.temp temperature at which measurement was taken or previously scaled to
##' @param new.temp temperature to be scaled to, default = 25 C
##' @return numeric value at reference temperature
##' @export
##' @author unknown
arrhenius.scaling <- function(observed.value, old.temp, new.temp = 25) {
  new.temp.K <- ud_convert(new.temp, "degC", "K")
  old.temp.K <- ud_convert(old.temp, "degC", "K")
  return(observed.value / exp(3000 * (1 / (new.temp.K) - 1 / (old.temp.K))))
} # arrhenius.scaling
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' Capitalize a string
##'
##' @title Capitalize a string
##' @param x string
##' @return x, capitalized
##' @author David LeBauer
capitalize <- function(x) {
  x <- as.character(x)
  s <- strsplit(x, " ")[[1]]
  return(paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " "))
} # capitalize

# isFALSE <- function(x) !isTRUE(x)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' New xtable
##'
##' utility to properly escape the '%' sign for latex
##' @title newxtable
##' @param x data.frame to be converted to latex table
##' @param environment can be 'table'; 'sidewaystable' if using latex rotating package
##' @param table.placement,label,caption,caption.placement,align passed to \code{\link[xtable]{xtable}}
##' @return Latex version of table, with percentages properly formatted
##' @author David LeBauer
newxtable <- function(x, environment = "table", table.placement = "ht", label = NULL,
                      caption = NULL, caption.placement = NULL, align = NULL) {
  need_packages("xtable")
  print(xtable::xtable(x, label = label, caption = caption, align = align),
        floating.environment = environment,
        table.placement = table.placement,
        caption.placement = caption.placement,
        #        sanitize.text.function = function(x) gsub("%", "\\\\%", x),
        sanitize.rownames.function = function(x) paste(''))
} # newxtable
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' Convert author, year, title to bibtex citation format
##'
##' Converts author year title to author1999abc format
##' @title bibtexify
##' @param author name of first author
##' @param year year of publication
##' @param title manuscript title
##' @return bibtex citation
##' @author unknown
bibtexify <- function(author, year, title) {
  acronym <- abbreviate(title, minlength = 3, strict = TRUE)
  return(paste0(author, year, acronym))
} # bibtexify
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' Convert categorical variable into sequential integers
##'
##' Turns any categorical variable into a sequential integer.
##' This transformation is required for using data in BUGS/JAGS
##' @title as.sequence
##' @param x categorical variable as vector
##' @param na.rm logical: return NA's or replace with max(x) + 1
##' @return sequence from 1:length(unique(x))
##' @export
##' @author David LeBauer
as.sequence <- function(x, na.rm = TRUE) {
  x2 <- as.integer(factor(x, unique(x)))
  if (all(is.na(x2))) {
    x2 <- rep(1, length(x2))
  }
  if (na.rm == TRUE) {
    x2[is.na(x2)] <- max(x2, na.rm = TRUE) + 1
  }
  return(x2)
} # as.sequence
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' Create a temporary settings file
##'
##' Uses \code{\link{tempfile}} function to provide a valid temporary file (OS independent)
##' Useful for testing functions that depend on settings file
##' Reference: http://stackoverflow.com/a/12940705/199217
##' @title temp.settings
##' @param settings.txt character vector to be written
##' @return character vector written to and read from a temporary file
##' @export
##' @author David LeBauer
temp.settings <- function(settings.txt) {
  temp <- tempfile()
  on.exit(unlink(temp), add = TRUE)
  writeLines(settings.txt, con = temp)
  settings <- readLines(temp)
  return(settings)
} # temp.settings
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' Test if function gives an error
##'
##' adaptation of try that returns a logical value (FALSE if error)
##' @title tryl
##' @param FUN function to be evaluated for error
##' @return FALSE if function returns error; else TRUE
##' @export
##' @examples
##' tryl(1+1)
##' # TRUE
##' tryl(sum('a'))
##' # FALSE
##' @author David LeBauer
tryl <- function(FUN) {
  out <- tryCatch(FUN, error = function(e) e)
  ans <- !inherits(out, "error")
  return(ans)
} # tryl
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' load model package
##' @title Load model package
##' @param model name of model
##' @return FALSE if function returns error; else TRUE
##' @export
##' @examples
##' \dontrun{require.modelpkg(BioCro)}
##' @author David LeBauer
load.modelpkg <- function(model) {
  pecan.modelpkg <- paste0("PEcAn.", model)
  if (!pecan.modelpkg %in% names(utils::sessionInfo()$otherPkgs)) {
    if (pecan.modelpkg %in% rownames(utils::installed.packages())) {
      do.call(require, args = list(pecan.modelpkg))
    } else {
      PEcAn.logger::logger.error("I can't find a package for the ", model,
                                 "model; I expect it to be named ", pecan.modelpkg)
    }
  }
} # load.modelpkg
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' conversion function for the unit conversions that udunits cannot handle but often needed in PEcAn calculations
##' @title misc.convert
##' @export
##' @param x convertible values
##' @param u1 unit to be converted from, character
##' @param u2 unit to be converted to, character
##' @return val converted values
##' @author Istem Fer, Shawn Serbin
misc.convert <- function(x, u1, u2) {
  
  amC   <- 12.0107  # atomic mass of carbon
  mmH2O <- 18.01528 # molar mass of H2O, g/mol
  
  if (u1 == "umol C m-2 s-1" & u2 == "kg C m-2 s-1") {
    val <- ud_convert(x, "ug", "kg") * amC
  } else if (u1 == "kg C m-2 s-1" & u2 == "umol C m-2 s-1") {
    val <- ud_convert(x, "kg", "ug") / amC
  } else if (u1 == "mol H2O m-2 s-1" & u2 == "kg H2O m-2 s-1") {
    val <- ud_convert(x, "g", "kg") * mmH2O
  } else if (u1 == "kg H2O m-2 s-1" & u2 == "mol H2O m-2 s-1") {
    val <- ud_convert(x, "kg", "g") / mmH2O
  } else if (u1 == "Mg ha-1" & u2 == "kg C m-2") {
    val <- x * ud_convert(1, "Mg", "kg") * ud_convert(1, "ha-1", "m-2")
  } else if (u1 == "kg C m-2" & u2 == "Mg ha-1") {
    val <- x * ud_convert(1, "kg", "Mg") * ud_convert(1, "m-2", "ha-1")
  } else {
    u1 <- gsub("gC","g*12",u1)
    u2 <- gsub("gC","g*12",u2)
    val <- ud_convert(x,u1,u2)
    
    
    #    PEcAn.logger::logger.severe(paste("Unknown units", u1, u2))
  }
  return(val)
} # misc.convert
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' function to check whether units are convertible by misc.convert function
##' @title misc.are.convertible
##' @export
##' @param u1 unit to be converted from, character
##' @param u2 unit to be converted to, character
##' @return logical
##' @author Istem Fer, Shawn Serbin
misc.are.convertible <- function(u1, u2) {
  
  # make sure the order of vectors match
  units.from <- c("umol C m-2 s-1", "kg C m-2 s-1",
                  "mol H2O m-2 s-1", "kg H2O m-2 s-1",
                  "Mg ha-1", "kg C m-2")
  units.to <- c("kg C m-2 s-1", "umol C m-2 s-1",
                "kg H2O m-2 s-1", "mol H2O m-2 s-1",
                "kg C m-2", "Mg ha-1")
  
  if(u1 %in% units.from & u2 %in% units.to) {
    if (which(units.from == u1) == which(units.to == u2)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' Convert expression to variable names
##' @title convert.expr
##' @param expression expression string
##' @return list
##' @export
##' @author Istem Fer
convert.expr <- function(expression) {
  # split equation to LHS and RHS
  deri.var <- gsub("=.*$", "", expression) # name of the derived variable
  deri.eqn <- gsub(".*=", "", expression) # derivation eqn
  
  non.match <- gregexpr('[^a-zA-Z_.]', deri.eqn) # match characters that are not "a-zA-Z_."
  split.chars <- unlist(regmatches(deri.eqn, non.match)) # where to split at
  # split the expression to retrieve variable names to be used in read.output
  if(length(split.chars)!=0){
    variables <- unlist(strsplit(deri.eqn, paste0("[",noquote(paste0(split.chars, collapse="")),"]")))
    variables <- variables[variables != ""] # Remove empty entries
  } else {
    variables <- deri.eqn
  }
  
  return(list(variable.drv = deri.var, variable.eqn = list(variables = variables, expression = deri.eqn)))
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' Simple function to use ncftpget for FTP downloads behind a firewall.
##' Requires ncftpget and a properly formatted config file in the users
##' home directory
##' @title download_file
##' @param url complete URL for file download
##' @param filename destination file name
##' @param method Method of file retrieval. Can set this using the `options(download.ftp.method=[method])` in your Rprofile.
##' example options(download.ftp.method="ncftpget")
##'
##' @examples
##' \dontrun{
##' download_file("http://lib.stat.cmu.edu/datasets/csb/ch11b.txt","~/test.download.txt")
##'
##' download_file("
##'   ftp://ftp.cdc.noaa.gov/Datasets/NARR/monolevel/pres.sfc.2000.nc",
##'   "~/pres.sfc.2000.nc")
##' }
##'
##' @export
##'
##' @author Shawn Serbin, Rob Kooper
download_file <- function(url, filename, method) {
  if (startsWith(url, "ftp://")) {
    if (missing(method)) method <- getOption("download.ftp.method", default = "auto")
    if (method == "ncftpget") {
      PEcAn.logger::logger.debug(paste0("FTP Method: ",method))
      #system2("ncftpget", c("-c", "url", ">", filename))
      system(paste(method,"-c",url,">",filename,sep=" "))
    } else {
      utils::download.file(url, filename, method)
    }
  } else {
    utils::download.file(url, filename)
  }
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' Retry function X times before stopping in error
##'
##' @title retry.func
##' @name retry.func
##' @description Retry function X times before stopping in error
##'
##' @param expr The function to try running
##' @param maxErrors The number of times to retry the function
##' @param sleep How long to wait before retrying the function call
##' @param isError function to use for checking whether to try again.
##'   Must take one argument that contains the result of evaluating `expr`
##'   and return TRUE if another retry is needed 
##'
##' @return retval returns the results of the function call
##'
##' @examples
##' \dontrun{
##'   file_url <- paste0("https://thredds.daac.ornl.gov/", 
##'       "thredds/dodsC/ornldaac/1220", 
##'       "/mstmip_driver_global_hd_climate_lwdown_1999_v1.nc4")
##' dap <- retry.func(
##'   ncdf4::nc_open(file_url),
##'   maxErrors=10,
##'   sleep=2)
##' }
##'
##' @export
##' @author Shawn Serbin <adapted from https://stackoverflow.com/questions/20770497/how-to-retry-a-statement-on-error>
retry.func <- function(expr, isError = function(x) inherits(x, "try-error"), maxErrors = 5, sleep = 0) {
  attempts = 0
  retval = try(eval(expr))
  while (isError(retval)) {
    attempts = attempts + 1
    if (attempts >= maxErrors) {
      msg = sprintf("retry: too many retries [[%s]]", utils::capture.output(utils::str(retval)))
      PEcAn.logger::logger.warn(msg)
      stop(msg)
    } else {
      msg = sprintf("retry: error in attempt %i/%i [[%s]]", attempts, maxErrors,
                    utils::capture.output(utils::str(retval)))
      PEcAn.logger::logger.warn(msg)
      #warning(msg)
    }
    if (sleep > 0) Sys.sleep(sleep)
    retval = try(eval(expr))
  }
  return(retval)
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' Adverb to try calling a function `n` times before giving up
##'
##' @param .f Function to call.
##' @param n Number of attempts to try
##' @param timeout Timeout between attempts, in seconds
##' @param silent Silence error messages?
##' @return Modified version of input function
##' @examples
##' rlog <- robustly(log, timeout = 0.3)
##' try(rlog("fail"))
##' \dontrun{
##'  nc_openr <- robustly(ncdf4::nc_open, n = 10, timeout = 0.5)
##'  nc <- nc_openr(url)
##'  # ...or just call the function directly
##'  nc <- robustly(ncdf4::nc_open, n = 20)(url)
##'  # Useful in `purrr` maps
##'  many_vars <- purrr::map(varnames, robustly(ncdf4::ncvar_get), nc = nc)
##' }
##' @export
robustly <- function(.f, n = 10, timeout = 0.2, silent = TRUE) {
  .f <- purrr::as_mapper(.f)
  function(...) {
    attempt <- 1
    while (attempt <= n) {
      result <- try(.f(...), silent = silent)
      if (!inherits(result, "try-error")) return(result)
      attempt <- attempt + 1
      if (!silent) PEcAn.logger::logger.info("Trying attempt ", attempt, " of ", n)
    }
    PEcAn.logger::logger.severe("Failed after", n, "attempts.")
  }
}
#--------------------------------------------------------------------------------------------------#


####################################################################################################
### EOF.  End of R script file.
####################################################################################################
