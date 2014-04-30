#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Returns the fully qualified hostname. This is potentially different from Sys.info()['nodename']
##' which can return just the hostname part and not the domain as well. For example the machine
##' pecan.ncsa.illinois.edu will return just that as fqdn but only pecan for hostname.
##'
##' @title Returns the fully qualified hostname.
##' @name fqdn
##' @author Rob Kooper
##' @return fully qualified hostname
##' @export
##' @examples
##' fqdn()
fqdn <- function() {
	system2('hostname', '-f', stdout=TRUE)
}
