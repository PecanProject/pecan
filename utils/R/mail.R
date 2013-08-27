#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------#
##' Sends email. This assumes the program sendmail is installed. 
##'
##' @title Clear EBI-CLUSTER worker node local scratch directories of old PEcAn output
##' @name sendmail
##' @param from the sender of the mail message
##' @param to the receipient of the mail message
##' @param subject the subject of the mail message
##' @param body the body of the mail message
##' @author Rob Kooper
##' @return nothing
##' @export
##' @examples
##' \dontrun{
##' sendmail("bob@@example.com", "joe@@example.com", "Hi", "This is R.")
##' }
sendmail <- function(from, to, subject, body) {
  if (is.null(to)) {
    logger.error("No receipient specified, mail is not send.")
  } else {
    if (is.null(from)) {
      from <- to
    }
    mailfile <- tempfile("mail")
    cat(paste0("From: ", from, "\n",
               "Subject: ", subject, "\n",
               "To: ", to, "\n",
               "\n",
               body), file=mailfile)
    system2("sendmail", c("-f", paste0('"', from, '"'), paste0('"', to, '"'), "<", mailfile))
    unlink(mailfile)
  }
}

