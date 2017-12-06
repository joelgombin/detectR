#' Write a df to the parquet file format
#' 
#' @export
#' @param x the table to save 
#' @param path the relative path within the parquet store
#' @param db a drill db connection
#' @value silently returns the `x` object
write_to_parquet <- function(x, path, db) {
  dir.create(paste0("/data/tmp/", path))
  tmp <- paste0("/data/tmp/", path,"/data.csvh")
  readr::write_csv(x, tmp)
  query <- paste0("CREATE TABLE dfs.tmp.`/parquet/", path, "/data.parquet`  AS SELECT * FROM dfs.tmp.`", paste0("/tmp/", path, "/data.csvh"), "`")
  DBI::dbGetQuery(db$con, 
             query
  )
  unlink(tmp)
  invisible(x)
}

#' Read from a parquet file system
#' 
#' @export
#' 
read_from_parquet <- function(path, db, columns = "*", where = NULL) {
  
  query <- paste0("SELECT ", paste0(columns, collapse = ", "), " FROM dfs.tmp.`/parquet/", path, "/*.parquet`")
  if (!is.null(where)) {
    query <- paste0(query, " WHERE ", where)
  }
  
  tibble::as_tibble(dbGetQuery(db$con,
             query))
}
