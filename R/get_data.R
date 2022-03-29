get_data <- function(query) {
  # connect to the database
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver       = "/opt/amazon/redshiftodbc/lib/64/libamazonredshiftodbc64.so",
                        servername   = Sys.getenv("REDSHIFT_SERVERNAME"),
                        database     = Sys.getenv("REDSHIFT_DATABASE"),
                        UID          = Sys.getenv("REDSHIFT_ACCESS_KEY_ID"),
                        PWD          = Sys.getenv("REDSHIFT_SECRET_ACCESS_KEY"),
                        Port         = 5439,
                        encoding     = "UTF-8")

  # get query
  data <- DBI::dbGetQuery(con, dplyr::sql(query))

  # desconnect from dataframe
  DBI::dbDisconnect(con)

  return(data)
}