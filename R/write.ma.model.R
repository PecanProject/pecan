##  write.ma.model ()
##      inserts prior distribution name and parameter values
##      adapted from R2WinBUGS write.model(
## The function is currently defined as
write.ma.model <- function (modelfile, outfile, reg.model,
                            pr.dist, pr.param.a, pr.param.b,
                            n, trt.n, site.n, ghs.n) {

  model.text <- scan(file=modelfile, what="character",sep="@")
  ## chose an uncommon separator in order to capture whole lines

  model.text <- gsub("%_%", "", model.text)
  model.text <- gsub("REGMODEL", reg.model, model.text)
  model.text <- gsub("PRIORDIST", paste("d", pr.dist,sep=""), model.text)
  model.text <- gsub("PRIORPARAMA", pr.param.a, model.text)
  model.text <- gsub("PRIORPARAMB", pr.param.b, model.text)
  model.text <- gsub("LENGTHK", n, model.text)
  model.text <- gsub("LENGTHJ", trt.n, model.text)
  model.text <- gsub("LENGTHG", site.n, model.text)
  if(ghs.n == 1)  model.text <- gsub("\\#GGG", '\\#', model.text)
  if(site.n == 1) model.text <- gsub("\\#SSS", '\\#', model.text)
  if(trt.n == 1)  model.text <- gsub("\\#TTT", '\\#', model.text)
  if(ghs.n > 1)   model.text <- gsub("\\#GGG", '', model.text)
  if(site.n > 1)  model.text <- gsub("\\#SSS", '', model.text)
  if(trt.n > 1)   model.text <- gsub("\\#TTT", '', model.text)
  writeLines(model.text, con = outfile)
}
