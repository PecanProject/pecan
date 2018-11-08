library(pecanapi)
import::from(magrittr, "%>%")

# Establish database connection
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  user = "bety",
  password = "bety",
  host = "localhost",
  port = 5432
)

model_id <- get_model_id(con, "SIPNET", "136")
all_umbs <- search_sites(con, "umbs%disturbance")
site_id <- subset(all_umbs, !is.na(mat))[["id"]]
workflow <- insert_new_workflow(con, site_id, model_id,
                                start_date = "2004-01-01",
                                end_date = "2004-12-31")

settings <- list() %>%
  add_workflow(workflow) %>%
  add_database() %>%
  add_pft("temperate.deciduous") %>%
  add_rabbitmq(con = con) %>%
  modifyList(list(
    meta.analysis = list(iter = 3000, random.effects = FALSE),
    run = list(inputs = list(met = list(source = "CRUNCEP", output = "SIPNET", method = "ncss"))),
    ensemble = list(size = 1, variable = "NPP")
  ))

submit_workflow(settings)
