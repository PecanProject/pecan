# ViewMet Server 
lapply(c( "shiny",
          "ggplot2",
          "stringr",
          "ncdf4",
          "ncdf4.helpers",
          "DT",
          "plyr",
          "dplyr"),function(pkg){
            if (!(pkg %in% installed.packages()[,1])){
              install.packages(pkg)
            }
            library(pkg,character.only = TRUE,quietly = TRUE)
          }
)

lapply(c( "PEcAn.benchmark",
          "PEcAn.visualization",
          "PEcAn.logger",
          "PEcAn.remote"),function(pkg){
            library(pkg,character.only = TRUE,quietly = TRUE)
          }
)



options(shiny.maxRequestSize=30*1024^2) #maximum file input size

server <- function(input, output, session) {
  
  bety <- betyConnect()
  rv <- reactiveValues()
  
  observe({
    # Look in the database for all inputs that are in CF format 
    # and grab their site ids
    inputs <- tbl(bety, "inputs") %>% filter(format_id == 33) %>% collect 
    updateSelectizeInput(session, "site.id", choices = sort(unique(inputs$site_id)))
    
  })
  
  
  observeEvent({input$site.id}, ignoreInit = TRUE, {
    
    site <- input$site.id
    PEcAn.logger::logger.debug("Site", site, "selected")
    
    if(is.na(as.numeric(site))){
      full.paths <- ""
    }else{
      
      # Given site id, get info about the input 
      inputs <- tbl(bety, "inputs") %>% filter(format_id == 33) %>% 
        filter(site_id == site) %>% collect
      
      # Check the machine
      # If the machine is one of the three pecan servers then files that have
      # dbfile entries for any of the three servers can be loaded 
      host <- PEcAn.remote::fqdn()
      if(host %in% c("test-pecan.bu.edu", "pecan1.bu.edu", "pecan2.bu.edu")){
        host <- c("test-pecan.bu.edu", "pecan1.bu.edu", "pecan2.bu.edu")
      }
      machine <- tbl(bety, "machines") %>% filter(hostname %in% host) %>% collect
      
      dbfiles <- tbl(bety, "dbfiles") %>%
        filter(container_type == "Input") %>%
        filter(container_id %in% inputs$id) %>%
        filter(machine_id %in% machine$id) %>%
        collect 
      
      if(all(dim(dbfiles) == 0)){
        full.paths <- ""
      }else{
        dbfiles <- dbfiles %>%
          mutate(file = gsub("//", "/",file.path(file_path, file_name)))
        
        types <- unique(dbfiles$file_path) %>% basename() %>%
          gsub(pattern = "\\_site_.*",replacement = "", x = .) %>%
          unique() %>% sort()
        
        updateCheckboxGroupInput(session, "met", choices = types)
        
        paths <- unique(dbfiles$file)
        full.paths <- c()
        yrs <- c()
        for(i in seq_along(paths)){
          new.files <- dir(dirname(paths[i]), pattern = basename(paths[i]),
                          full.names = TRUE)
          yrs <- c(yrs, stringr::str_extract(new.files, pattern="[0-9]{4}"))
          full.paths <- c(full.paths,new.files)
        }
        updateCheckboxGroupInput(session, "years", choices = sort(unique(yrs)))
      }
    }
    rv$full.paths <- full.paths
    
  })
  
  # Once met and years are selected, the paths to available files on the server 
  # will show in a tabel
  observeEvent({
    input$met 
    input$years},{
    
    met <- input$met
    years <- input$years
    
    full.paths <- rv$full.paths

    load.paths <- c()
    for(i in seq_along(met)){
      new.paths <- full.paths[grep(paste0(met[i],"_site_"), full.paths)]
      year_sub <- stringr::str_extract(new.paths, pattern="[0-9]{4}") %in% years
      new.paths <- new.paths[year_sub]
      load.paths <- c(load.paths, new.paths)
    }
    rv$load.paths <- load.paths
  })
  
  
  observeEvent({rv$load.paths},{
    output$results_table <- DT::renderDataTable(DT::datatable(as.matrix(rv$load.paths)))
  })
  
  # Click the load data to read in the met data
  load.model.data <- eventReactive(input$load_data, {
    req(input$met)
    req(input$years)
    
    PEcAn.logger::logger.debug("Loading", input$met)
    PEcAn.logger::logger.debug("Loading", input$years)
    
    data <- list()
    for(i in seq_along(rv$load.paths)){
      
      fpath <- dirname(rv$load.paths[i])
      inputid <- tbl(bety, "dbfiles") %>% filter(file_path == fpath) %>% 
        pull(container_id) %>% unique() %>% .[1]
      formatid <- tbl(bety, "inputs") %>% filter(id == inputid) %>% pull(format_id)
      siteid <- tbl(bety, "inputs") %>% filter(id == inputid) %>% pull(site_id)
      
      site = query.site(con = bety$con, siteid)
      
      vars_in_file <- ncdf4::nc_open(rv$load.paths[i]) %>% ncdf4.helpers::nc.get.variable.list()
      format = query.format.vars(bety, inputid, formatid)
      format$vars <- format$vars %>% filter(input_name %in% vars_in_file)
      
      
      dat <- try(load_data(data.path = rv$load.paths[i],
                           format = format, site = site, ))
      
      if(inherits(dat, "data.frame")) {
        dat$met <- rv$load.paths[i] %>% dirname() %>% basename() %>%
          gsub(pattern = "\\_site_.*",replacement = "", x = .)
        data[[i]] <- dat
      }
    }
    data.final <- do.call(plyr::rbind.fill, data)
    return(data.final)
  })
  
  observeEvent(input$load_data, {
    data.final <- load.model.data()
    rv$data.final <- data.final
    updateSelectizeInput(session, "var", choices = colnames(data.final))
  })
  
  observeEvent({input$var},{
    PEcAn.logger::logger.debug(input$var)
    var <- input$var
    if(input$var != ""){
      plot.data <- rv$data.final %>% dplyr::select(one_of(c("posix", var, "met")))
      # print(head(plot.data))
      colnames(plot.data) <- c("date", "var", "met")
      rv$plot.data <- plot.data
    }
  })
  
  observeEvent(input$plot_data,{
    req(rv$plot.data)
    req(input$var)

    rv$plot.data$met <- factor(rv$plot.data$met, 
           levels = sort(unique(rv$plot.data$met), decreasing = TRUE))
  
    p_overlay <- ggplot(rv$plot.data) + geom_line(aes(x=date, y=var, color=met)) + 
      ylab(input$var) + ggtitle(input$var)
    
    p_facet <-   ggplot(rv$plot.data) + geom_line(aes(x=date, y=var, color=met), size=1) + 
      ylab(input$var) + ggtitle(input$var) + 
      facet_grid(met ~ .)
    
    output$plot_overlay <- renderPlot(p_overlay)
    PEcAn.logger::logger.debug("Overlay plot finished")
    output$plot_facet <- renderPlot(p_facet)
    PEcAn.logger::logger.debug("Facet plot finished")
  
  })



}