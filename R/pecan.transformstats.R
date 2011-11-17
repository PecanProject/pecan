
pecan.transformstats <- function(data) {
  ## Transformation of stats to SE
  ## transform SD to SE
  if (max(c("SD","sd") %in% data$statname)) {
    sdi <- which(data$statname %in% c("SD","sd"))
    data$stat[sdi] <- data$stat[sdi] / sqrt(data$n[sdi])
    data$statname[sdi] <- "SE"
  }
  ## transform MSE to SE
  if ("MSE" %in% data$statname) {
    msei <- which(data$statname == "MSE")
    data$stat[msei] <- sqrt (data$stat[msei]/data$n[msei])
    data$statname[msei] <- "SE"
  }
  ## 95%CI measured from mean to upper or lower CI
  ## SE = CI/t
  if ("95%CI" %in% data$statname) {
    cii <- which(data$statname == '95%CI')
    data$stat[cii] <- data$stat[cii]/qt(0.975,data$n[cii])
    data$statname[cii] <- "SE"
  }
  ## Fisher's Least Significant Difference (LSD)
  ## conservatively assume no within block replication
  if ("LSD" %in% data$statname) {
    lsdi <- which(data$statname == "LSD")
    data$stat[lsdi] <- data$stat[lsdi] / (qt(0.975,data$n[lsdi]) * sqrt( (2 * data$n[lsdi])))
    data$statname[lsdi] <- "SE"
  }
  ## Tukey's Honestly Significant Difference (HSD),
  ## conservatively assuming 3 groups being tested so df =2
  if ("HSD" %in% data$statname) {
    hsdi <- which(data$statname == "HSD")
    n = data$n[hsdi]
    n[is.na(n)] = 2 ## minimum n that can be used if NA
    data$stat[hsdi] <- data$stat[hsdi] / (qtukey(0.975, n, df = 2))
    data$statname[hsdi] <- "SE"
    data$n[hsdi] <- n
  }              
  ## MSD Minimum Squared Difference
  ## MSD = t_{\alpha/2, 2n-2}*SD*sqrt(2/n)
  ## SE  = MSD*n/(t*sqrt(2))
  if ("MSD" %in% data$statname) {
    msdi <- which(data$statname == "MSD")
    data$stat[msdi] <- data$stat[msdi] * data$n[msdi] / ( qt(0.975,2*data$n[msdi]-2)*sqrt(2))
    data$statname[msdi] <- "SE"
  }
  if (FALSE %in% c('SE','none') %in% data$statname) {
    print(paste(trait, ': ERROR!!! data contains untransformed statistics'))
  }
  return(data)
}

#' @example data <- data.frame(Y=rep(1,5), stat=rep(1,5), n=rep(4,5), statname=c('SD', 'MSE', 'LSD', 'HSD', 'MSD') 
