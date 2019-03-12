#' export from DBs to CSV
#' 
#' @export 
#' @import progress
#' @param conn a connection to the database from which the data is retrieved
#' @param table the table name from which the data is retrieved 
#' @param output_path the path where the dump will be saved 
#' @return NULL

export_csv <- function(conn, table, output_path = paste0("/data/detectR/dumps/", table, ".csv"), chunk_length = 1000000) {
  
  chunk_length <- as.integer(chunk_length)
  query <- glue::glue_sql("SELECT * FROM {`table`} LIMIT {chunk_length}",
                          .con = conn)
  rs <- DBI::dbSendQuery(conn, query)
  i <- 1
  
  if (file.exists(output_path)) {
    message("Fichier existant supprimé")
    file.remove(output_path)
  }
  message("Sauvegarde de la table ", table, "\n")
  n <- dplyr::tbl(conn, table) %>% summarise(n = n()) %>% pull(n)
  
  pb <- progress_bar$new(total = n, show_after = 0)
  pb$tick(0)
  
  while (!DBI::dbHasCompleted(rs)) {
    chunk <- DBI::dbFetch(rs)
    
    ## the query works even if there is no result any more
    if(nrow(chunk) == 0){
      break
    }
    
    ## save as csv
    
    if (i %in% 1) {
      readr::write_csv(chunk, path = output_path, col_names = TRUE, append = FALSE)
    } else {
      readr::write_csv(chunk, path = output_path, append = TRUE)
    }
    
    DBI::dbClearResult(rs)
    offset <- bit64::as.integer64(chunk_length * i)
    cat("i = ", i, " ; offset = ", offset, "\n")
    query <- glue::glue_sql("SELECT * FROM {`table`} LIMIT {chunk_length} OFFSET {offset}",
                            .con = conn)
    rs <- DBI::dbSendQuery(conn, query)
    i <- i + 1
    gc()
    pb$update(min(offset / n, 1))
  }
  DBI::dbClearResult(rs)
} 
