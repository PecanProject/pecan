Steps for running and adding SIPNET forecasts 
==========================================================================
 
### Files associated with running forecasts:
1. `generate.gefs.xml` : R script for setting start and end date in xml; creates workflow id and associated directory
2. `*sitename*.xml` : site specific xml script 
3. `*sitename*.sh` : site specific shell file to run forecasts with cron jobs 

### Files associated with graphing forecasts: 
1. `download_*sitename*.R` : R script for downloading and cleaning Flux data from towers 
2. `graph_fluxtowers.R` : script to create RData for shiny app 
3. `forecast.graphs.R` : script that is used to get forecasted data for `graph_fluxtowers.R`
4. `graph_SDA_fluxtowers.R` : script to graph SDA fluxtowers 
5. `automatic_graphs.sh` : shell for running `email.R`, `graph_fluxtowers.R`, `graph_SDA_fluxtowers.R`

### Files for automatic WCr emails: 
1. `email_graphs.R` - creates graphs for the email - this is specifically for WCr right now
2. `email.R` - sends the email out with graphs for today's forecast




### To add forecast and add to dashboard: 
1. create a flux download for site
2. create site specific xml 
3. create site specific shell file to run forecast automatically 
4. Add site.num, site.abv, outdir, db.num to graph_fluxtowers.R 

