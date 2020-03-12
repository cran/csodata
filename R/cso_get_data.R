#' Return a CSO table as a data frame
#'
#' Returns a CSO table from the CSO StatBank Application Programming Interface
#' (API) as a data frame, with the option to give it in wide format (default)
#' very wide or long/tidy format.
#'
#' The data is pulled from the ResponseInstance service on the CSO API in
#' JSON-Stat fromat, using the GET method from the httr package.
#'
#' @param table_code string. If the table_code is a filename or a path to a
#' file, e.g. "QNQ22.json", it is imported from that file. Otherwise if it is
#' only a table code e.g. "QNQ22", the file is downloaded from StatBank and
#' checked to see if it is a valid table.
#' @param wide_format string, one of "wide", "very_wide" or "tall". If "wide"
#' (default) the table is returned in wide (human readable) format, with
#' statistic as a column (if it exists). If "very_wide" the table is returned
#' wide format and spreads the statistic column to rows. If "tall" the table is
#' returned in tall/tidy format.
#' @param include_ids logical. The JSON-stat format stores variables as ids
#' i.e. IE11 and labels i.e. Border. While the label is generally preferred,
#' sometimes it is useful to have the ids to match on. If \code{include_ids}
#' is TRUE (default) then ids are retrieved and appended to the table to the
#' right of the original column with the name <columnName>.id.
#' @param id_list either NULL (default) or a character vector of columns that
#' should have ids appended if include_ids is TRUE.
#' if NULL then every column that is not included in the vector
#' \code{remove_id} will be used.
#' @param use_factors logical. If TRUE (default) factors will be used in
#' strings.
#' @param cache logical. if TRUE (default) csodata will cache the result using
#' R.cache. The raw data downloaded from the statbank is cached, which means
#' that calling \code{cso_get_data} with the same table_code but different
#' parameterswill result in cached data being used.
#' @return data frame of the requested CSO table.
#' @export
#' @examples
#' \dontrun{
#' tbl1 <- cso_get_data("QNQ22")
#' tbl2 <- cso_get_data("QLF07.json")
#' }
cso_get_data <- function(table_code, wide_format = "wide", include_ids = TRUE,
                         id_list = NULL, use_factors = TRUE, cache = TRUE) {
  # Set path to or download data --------
  if (substr(
    table_code, nchar(table_code) - 4, nchar(table_code)
  ) == ".json") {
    if (file.exists(table_code)) {
      json_data <- table_code
    } else {
      stop("Not a valid path to a .json file")
    }
  } else {
    json_data <- cso_download_tbl(table_code, cache = cache)
    # Error Checking ----------------------
    if (is.null(json_data)) {
      return(NULL)
    }
  }

  # Load data ---------------------------
  data <- rjstat::fromJSONstat(json_data,
    naming = "label", use_factors = use_factors
  )[[1]]
  names(data) <- make.names(names(data), unique = TRUE)

  # Append ids as new column ------------
  if (include_ids) {
    data_id <- rjstat::fromJSONstat(json_data,
      naming = "id", use_factors = use_factors
    )[[1]]
    names(data_id) <- make.names(names(data_id), unique = TRUE)

    if (is.null(id_list)) {
      join_list <- names(data_id)
      remove_id <- c(
        "Statistic", "Sector", "Year", "Quarter", "Month", "Item",
        "value", "CensusYear", "Census.Year", "HalfYear", "Sex", "Age",
        "Intercensal.Period"
      )
      concat_id <- join_list [!join_list %in% remove_id]
    } else {
      concat_id <- id_list
    }

    for (ID in concat_id) {
      id_list <- list(as.vector(as.matrix(data_id[ID])))
      names(id_list) <- paste0(ID, ".id")
      data <- data.frame(append(data, id_list, after = match(ID, names(data))),
        stringsAsFactors = use_factors
      )
    }
  }

  # Pivot to wide table -----------------
  if (wide_format == "wide" || wide_format == "very_wide") {
    string <- names(data)
    remove <- c(
      "Year", "Quarter", "Month", "value", "CensusYear",
      "Census.Year", "HalfYear", "Intercensal.Period"
    )

    if (wide_format == "very_wide") {
      remove <- append(remove, "Statistic")
    }

    row_vars <- string [!string %in% remove]

    wide_data <- reshape2::dcast(data, formula = paste(
      paste(row_vars, collapse = " + "), " ~ ... "
    ))

    if (!use_factors) {
      wide_data <- dplyr::mutate_if(wide_data, is.factor, as.character)
    }

    return(wide_data)
  } else {
    return(data)
  }
}

#' Download a CSO table as a data frame
#'
#' Internal function to return a CSO table from the CSO StatBank Application
#' Programming Interface (API) as a JSON-stat dataset.
#'
#' The data is pulled from the ResponseInstance service on the CSO API in
#' JSON-Stat fromat, using the GET method from the httr package.
#'
#' To improve performance, the result is cached by default.
#'
#' @param table_code string. The code uniquely identifying one table.
#' @param cache logical. Indicates whether to cache the result using R.cache.
#' @param suppress_messages logical. If FALSE (default) a message is printed
#' when loading a previously cached data table.
#' @return a character object, containing the data in JSON-stat format.
#' @noRd
cso_download_tbl <- function(table_code, cache = TRUE,
                             suppress_messages = FALSE) {
  url <- paste0(
    "https://statbank.cso.ie/StatbankServices/StatbankServices",
    ".svc/jsonservice/responseinstance/", table_code
  )

  # Attempt to retrieve cached data -----
  if (cache) {
    toc <- cso_get_toc(suppress_messages = TRUE)
    last_update <- toc[toc$id == table_code, 2]
    data <- R.cache::loadCache(list(table_code, last_update), dirs = "csodata")
    if (!is.null(data)) {
      if (!suppress_messages) {
        message("Loaded cached data\n")
      }
      return(data)
    }
  }

  # No caching, or cache empty ----------

  # Check for errors using trycatch since StatBank API does not support
  # html head requests.
  error_message =  paste0("Failed retrieving table. Please check internet ",
                          "connection and that statbank.cso.ie is online")

  response <- tryCatch({
    httr::GET(url)
  }, warning = function(w) {
    print(paste0("Warning: ", error_message))
    return(NULL)
  }, error = function(e) {
    print(paste0("Error: ", error_message))
    return(NULL)
  })

  # Check if data valid -------------
  if (httr::status_code(response) == 200 &&
    !all(response[["content"]][1:32] ==
      charToRaw("invalid Maintable format entered"))) {
    json_data <- rawToChar(response[["content"]])
    if (cache) {
      toc <- cso_get_toc(suppress_messages = TRUE)
      last_update <- toc[toc$id == table_code, 2]
      R.cache::saveCache(json_data,
        key = list(table_code, last_update), dirs = "csodata"
      )
    }
    return(json_data)
  } else {
    stop("Not a valid table code. See cso_get_toc() for all valid tables.")
  }
}
