####Send Emails of WCr Forecasts ####
library(EasyHTMLReport)

emails <- read.csv("/fs/data3/kzarada/NEFI/Willow_Creek/emails.csv", header = T)

for(i in 1:dim(emails)[1]){
from <- 'kzarada428@gmail.com'
to <- dput(as.character(emails$Email[i]))
subject <- paste0("Willow Creek Forecast for ", Sys.Date())
mailControl = list(host.name = 'smtp.gmail.com', port = 465, user.name = 'kzarada428', passwd = "Poland5178", ssl = T)

easyHtmlReport(rmd.file = "/fs/data3/kzarada/NEFI/Willow_Creek/forecast.Rmd", 
               from = from, 
               to = to, 
               subject = subject, 
               control = mailControl)

}





