#' Returns a data frame with the metadata of a CSO data table
#'
#' Checks the CSO Statbank API for a metadata on a dataset and returns it as a
#' list of metadata and contained statistics.
#'
#' The statistics are pulled using the \code{\link{cso_get_content}} function.
#'
#' @param table_code string. A valid code for a table from the StatBank.
#' @param cache_data logical. Whether to use cached data, if available.
#' Default value is TRUE.
#' @param cache_toc logical. Whether to use cached table of contents, if
#' available. Default value is TRUE.
#' @return list with eight elements:
#' \itemize{
#'   \item The title of the table.
#'   \item The source of the data.
#'   \item The date when the data was retrieved from StatBank.
#'   \item The time interval used in the data. (Census year, Quarter, Month)
#'   \item The units used (the R class of the value column)
#'   \item The date the table was last modified.
#'   \item The names of the variables included in the table, returned as a
#'   character vector with one element for each variable.
#'   \item The names of the statistics included in the table, returned as a
#'   character vector with one element for each statistic.
#' }
#' @export
#' @examples
#' \dontrun{
#' head(cso_get_meta("VSA16"))
#' meta1 <- cso_get_meta("HS014")
#' }
cso_get_meta <- function(table_code, cache_data = TRUE, cache_toc = TRUE) {
  # Use fromJSON in order to preserve metadata ---
  tbl <- cso_download_tbl(table_code, cache_data)
  # Error Checking ----------------------
  if (is.null(tbl)) {
    return(NULL)
  }

  response_fj <- jsonlite::fromJSON(tbl)

  stats <- cso_get_content(table_code)
  cso_toc <- cso_get_toc(cache_toc, suppress_messages = TRUE)

  title <- response_fj$dataset$label
  source <- response_fj$dataset$source
  date_downloaded <- response_fj$dataset$updated
  time_period <- response_fj$dataset$dimension$role$time
  units <- class(response_fj$dataset$value)
  date_modified <- cso_toc$LastModified[which(cso_toc$id == table_code)]
  vars <- setdiff(response_fj$dataset$dimension$id, "Statistic")

  list(
    Title = title, Source = source,
    Date_downloaded_from_StatBank = date_downloaded,
    Time = time_period, Units = units, Date_last_modified = date_modified,
    Variables = vars, Statistics = stats
  )
}


#' Returns a character vector listing the contents of a CSO data table
#'
#' Reads the metadata of a table to return a character vector of the
#' included variables in the table.
#'
#' @param table_code string. A valid code for one table from the
#' StatBank.
#' @param cache_data logical. Whether to use cached data, if available.
#' Default value is TRUE. Strongly recommended to use caching, as otherwise
#' the entire table could be downloaded only to access a small part of its
#' metadata.
#' @return character vector. The names of the statistics included in the
#' table, with one element for each statistic.
#' @export
#' @examples
#' cso_get_vars("IPA03")
cso_get_vars <- function(table_code, cache_data = TRUE) {
  tbl <- cso_download_tbl(table_code, cache_data)
  # Error Checking ----------------------
  if (is.null(tbl)) {
    return(NULL)
  }

  response_fj <- jsonlite::fromJSON(tbl)
  response_fj$dataset$dimension$id
}


#' Returns a list of the values of variables of a CSO data table
#'
#' Reads the table to determine all the unique values taken by the
#' variables in the table and returns them as a list.
#'
#' @param table_code string. A valid code for one table from the
#' StatBank.
#' @param cache_data logical. Whether to use cached data, if available.
#' Default value is TRUE.
#' @return list. It has length equal to the number of variables in the table,
#' and each element is a character vector which has all the values taken by
#' one variable.
#' @export
#' @examples
#' var_val <- cso_get_var_values("IPA03")
cso_get_var_values <- function(table_code, cache_data = TRUE) {

  tbl <- cso_download_tbl(table_code, cache_data)
  # Error Checking ----------------------
  if (is.null(tbl)) {
    return(NULL)
  }

  response_fj <- jsonlite::fromJSON(tbl)
  vars <- response_fj$dataset$dimension$id

  len <- length(vars)
  var_vec <- vector(mode = "list", length = len)
  names(var_vec) <- vars
  for (i in 1:len) {
    string <- paste0("response_fj$dataset$dimension$`",
                     vars[i], "`$category$label")
    var_vec[i] <-  list(as.vector(unlist(eval(parse(text = string)))))
  }

  return(var_vec)
}


#' Returns a the time interval used to record data in a CSO table
#'
#' Reads the metadata of a table to return an atomic character vector
#' displaying the intervals at which the data included in the table was
#' gathered/calculated.
#'
#' @param table_code string. A valid code for one table from the
#' StatBank.
#' @param cache_data logical. Whether to use cached data, if available.
#' Default value is TRUE. Strongly recommended to use caching, as otherwise
#' the entire table could be downloaded only to access a small part of its
#' metadata.
#' @return character vector. The names of the statistics included in the
#' table, with one element for each statistic.
#' @export
#' @examples
#' \dontrun{
#' cso_get_interval("C0636")
#' }
cso_get_interval <- function(table_code, cache_data = TRUE) {
  tbl <- cso_download_tbl(table_code, cache_data)
  # Error Checking ----------------------
  if (is.null(tbl)) {
    return(NULL)
  }

  response_fj <- jsonlite::fromJSON(tbl)

  if (!is.null(response_fj$dataset$dimension$role$time)) {
    out <- response_fj$dataset$dimension$role$time
  } else {
    out <- "There is no time interval information for this statbank table"
  }
  return(out)
}


#' Returns a character vector listing the statistics in a CSO data table
#'
#' Uses the ContentsIndicatorsList service of the CSO Statbank API to
#' returns a list the contained statistics for a table.
#' See \url{https://statbank.cso.ie/StatbankServices/StatbankServices.svc/
#' jsonservice/help/operations/ContentsIndicatorsList}
#' for more information on this.
#'
#' @param table_code string. A valid code for one table from the
#' StatBank.
#' @return character vector. The names of the statistics included in the
#' table, with one element for each statistic.
#' @export
#' @examples
#' cso_get_content("EP008")
cso_get_content <- function(table_code) {
  # Pull the list from API and keep only useful column -----
  url <- paste0(
    "https://statbank.cso.ie/StatbankServices/StatbankServices",
    ".svc/jsonservice/ContentsIndicatorsList/", table_code
  )

  # Check for errors using trycatch since StatBank API does not support
  # html head requests.
  error_message =  paste0("Failed retrieving list of contents. Please ",
          "check internet connection and that statbank.cso.ie is online")

  cont_list <- tryCatch({
    data.frame(jsonlite::fromJSON(url))
  }, warning = function(w) {
    print(paste0("Warning: ", error_message))
    return(NULL)
  }, error = function(e) {
    print(paste0("Error: ", error_message))
    return(NULL)
  })

  as.character(cont_list$ContentsIndicatorvalue)
}


#' Prints metadata from a StatBank table to the console
#'
#' Takes the output from \code{\link{cso_get_meta}} and prints it to the
#' console as formatted text.
#'
#' @param table_code string. A valid code for one table from the
#' StatBank.
#' @export
#' @examples
#' \dontrun{
#' cso_disp_meta("EP001")
#' }
cso_disp_meta <- function(table_code) {
  meta <- cso_get_meta(table_code)

  # Error Checking ----------------------
  if (is.null(meta)) {
    return(NULL)
  }

  message("*** METADATA ***\n")
  message("CSO Table = ", meta$Title, "\n")
  message("Units = ", meta$Units, "\n")
  message("Source = ", meta$Source, "\n")
  message("Time interval in data = ", meta$Time, "\n")
  message(
    "Date downloaded from statbank = ",
    meta$Date_downloaded_from_StatBank, "\n"
  )
  message("Date last modified = ", meta$Date_last_modified, "\n")
  message("Variables:")
  print(meta$Variables)
  message("\nStatistics:")
  print(meta$Statistics)
}
