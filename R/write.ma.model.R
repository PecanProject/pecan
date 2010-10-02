##  write.ma.model ()
##      inserts prior distribution name and parameter values
##      adapted from R2WinBUGS write.model(
## The function is currently defined as
write.ma.model <- function (ma.model, con = "model.bug", pr.dist,
pr.param.a, pr.param.b, total.n, total.trt, total.site){

 if (is.R()) {
   model.text <- attr(ma.model, "source")
   if(!is.null(ma.model) & is.null(model.text)){
     ## running from cmd line, attr doesn't work
     sink(".temp.jags")
     print(ma.model)
     sink()

     model.text <- scan(file=".temp.jags",what="character",sep="@")
             ## chose an uncommon separator in order to capture whole lines

   }
   model.text <- sub("^\\s*function\\s*\\(\\s*\\)", "model",
                     model.text)
 } else {
   model.text <- as.character(ma.model)
   model.text <- paste("model", model.text)
 }
 model.text <- gsub("%_%", "", model.text)
 model.text <- gsub("PRIORDIST", paste("d", pr.dist,sep=""), model.text)
 model.text <- gsub("PRIORPARAMA", pr.param.a, model.text)
 model.text <- gsub("PRIORPARAMB", pr.param.b, model.text)
 model.text <- gsub("LENGTHK", total.n, model.text)
 model.text <- gsub("LENGTHJ", total.trt, model.text)
 model.text <- gsub("LENGTHG", total.site, model.text)
 #model.text <- sub("model.", "model", model.text)
 if (!is.R()) {
   model.text <- replaceScientificNotation(model.text)
   model.text <- gsub("invisible[ ]*\\([ ]*\\)", "", model.text)
 }

 writeLines(model.text, con = con)
}
