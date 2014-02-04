test_biocLite_checkSvnRevision <- function()
{
    .checkSvnRevision <- BiocInstaller:::.checkSvnRevision

    checkException(.checkSvnRevision("55732"), silent=TRUE)
    checkException(.checkSvnRevision("55733"), silent=TRUE)
    checkTrue(.checkSvnRevision("55734"))

    ## no version number
    target <- "R.Version()[['svn rev']] == 'unknown' (expected > r55733)"
    msg <- NULL
    tryCatch(.checkSvnRevision("unknown"),
             warning=function(w) msg <<- conditionMessage(w))
    checkIdentical(target, msg)

    ## mixed source version numbers
    target <- "length(R.Version()[['svn rev']]) != 1"
    msg <- NULL
    tryCatch(.checkSvnRevision(c("55734", "55735")),
             warning=function(w) msg <<- conditionMessage(w))
    checkIdentical(target, msg)

    target <- "R.Version()[['svn rev']] ('adsf') cannot be coerced to integer"
    msg <- NULL
    tryCatch(.checkSvnRevision("adsf"),
             warning=function(w) msg <<- conditionMessage(w))
    checkIdentical(target, msg)
}
