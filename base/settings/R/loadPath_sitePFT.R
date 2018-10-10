#' Title loadPath.sitePFT
#'
#' @param settings pecan setting list.
#' @param Path  Character of file name with extension. The path will be generated using the outdir tag in pecan settings.
#'
#' @return a dataframe of two columns of site and pft
#' @export loadPath.sitePFT
#' @description The csv or the text file needs to have a header and be separated using comma. Under the first column in the text file, one needs to specify the site id and in the second column there has to be the name of the PFT.
#'
#' @examples
loadPath.sitePFT<-function(settings, Path){
  #finding the file extension.
  ext <- tools::file_ext(Path)
  
  if (ext=="csv" | ext=="txt"){
    # reading in the links
    links <- read.table(file.path(settings$outdir,Path), header = T, sep = ",")
    #check to make sure the input file is what we expect it.
    if (nrow(links)==0 | ncol(links)==0 | ncol(links)!=2) PEcAn.logger::logger.severe("There is a problem with reading the file. Either row number, column number is zero or your file does not have a two columns. ")
    
    return(links %>% `colnames<-`(c('site','pft')))
  }
  
}