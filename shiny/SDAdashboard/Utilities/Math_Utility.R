ts.producer.FA <-function(All.my.data, site_id, step=1){

site.cols<-which(attr(All.my.data[["FORECAST"]][[1]],'Site')==site_id)



output <- c('FORECAST','ANALYSIS')%>%
  purrr::map_df(function(listFA){
    curr_obj <-All.my.data[[listFA]]
    curr_obj<- curr_obj[1:step]
    curr_obj%>%
      purrr::map2_df(seq_along(curr_obj),function(state.vars,t){
        as.data.frame(state.vars) [,site.cols] %>%
          tidyr::gather(Variable, Value) %>%
          group_by(Variable) %>%
          summarise(
            Means=mean(Value, na.rm=T),
            Lower=quantile(Value,0.025, na.rm=T),
            Upper=quantile(Value,0.975, na.rm=T)
            ) %>%
          mutate(Time=t,
                 Type=listFA)
        
      })
  })
 
return(output)
}

expand.matrix <- function(X) {
  rows.c <- colnames(X)[(which(!(colnames(X) %in% row.names(X))))]
  rbind(X, matrix(0, length(rows.c), length(colnames(X))) %>%
          `rownames<-`(rows.c)) -> X2
  X2
}

generate_colors_sda <-function(){
  pink       <<- col2rgb("deeppink")
  alphapink  <<- rgb(pink[1], pink[2], pink[3], 180, max = 255)
  green      <<- col2rgb("green")
  alphagreen <<- rgb(green[1], green[2], green[3], 75, max = 255)
  blue       <<- col2rgb("blue")
  alphablue  <<- rgb(blue[1], blue[2], blue[3], 75, max = 255)
  purple       <<- col2rgb("purple")
  alphapurple <<- rgb(purple[1], purple[2], purple[3], 75, max = 255)
  brown       <<- col2rgb("brown")
  alphabrown <<- rgb(brown[1], brown[2], brown[3], 30, max = 255)
}
