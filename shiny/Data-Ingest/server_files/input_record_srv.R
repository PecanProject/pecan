######### Select Site ###############
bety <- betyConnect()

sites <- dplyr::tbl(bety, "sites") %>% 
            dplyr::select(one_of("sitename", "id")) %>% collect()

sitenames <- sites$id
names(sitenames) <- sites$sitename

updateSelectizeInput(session, "InputSiteID", choices = sort(unique(sitenames)))

output$siteIDout <- renderPrint()


####### Select Format ##############
formats <- dplyr::tbl(bety, "formats") %>% 
              dplyr::select(one_of("name", "id", "mimetype_id")) %>% collect()

selectformat <- formats$id 
names(selectformat) <- formats$name

updateSelectizeInput(session, "InputFormatID", choices = sort(unique(selectformat)))
