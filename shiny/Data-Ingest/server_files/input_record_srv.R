######### Select Site ###############
bety <- betyConnect()

sites <- dplyr::tbl(bety, "sites") %>% 
            dplyr::select(one_of("sitename", "id")) %>% collect()

sitenames <- sites$id
names(sitenames) <- sites$sitename

updateSelectizeInput(session, "InputSiteID", choices = sort(unique(sitenames)))

output$siteIDout <- renderPrint()