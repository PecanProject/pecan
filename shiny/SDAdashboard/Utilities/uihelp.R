jumbotron<-function(...){
  HTML(paste0("<div class='jumbotron'>",...,"</div>"))
}

card.light<-function(header="Header",title="Primary card title",border="light",txc=NULL,Tcol=NULL,...){
  classs<-paste0("card border-",border)
  bg<-ifelse(!is.null(Tcol),paste0('bg-',Tcol),"")
  classs<-paste(classs,bg)
  txcc<-ifelse(!is.null(txc),paste0('text-',txc),"")
  classs<-paste(classs,txcc)
  
  HTML(paste0('<div class="',classs,'" >',ifelse(!is.null(header),paste0('<div class="card-header"><h4><strong><b>',header,'</b></strong></h4></div>'),''),'
  <div class="card-body">
  <p class="card-text">',...,'</p>
                           </div>
                           </div>'))
}
empty.card<-function(...){

  withTags(
    div(class = "card",
        div(class="card-body",
            p(class="card-text",...)
        )
    )
  )
}
