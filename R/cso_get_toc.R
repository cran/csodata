#' Returns a data frame with all valid CSO Statbank tables listed sequentially
#' by id number, e.g. A0101, A0102, A0103, etc.
#'
#' Checks the CSO Statbank API for a list of all the table codes (e.g. A0101,
#' A0102, A0103, etc.), which also includes date last modified and title for
#' each table, and returns this list as an R data frame.
#'
#' The data is pulled from the TitledetailsList on the CSO API. See
#' \url{https://statbank.cso.ie/StatbankServices/StatbankServices.svc/
#' jsonservice/help/operations/TitledetailsList}
#' for more information on this.
#'
#' @param cache logical. If TRUE (default) the table of contents is cached
#' with the system date as a key.
#' @return data frame of three character columns:
#' \itemize{
#'   \item id. Contains all of the table codes currently
#' available on the CSO API.
#'   \item LastModified. The date the table was last modified in POSIXct
#'   format.
#'   \item title. The title of the table.
#' }
#'
#' @export
#' @examples
#' head(cso_get_toc())
cso_get_toc <- function(cache = TRUE) {
  url <- paste0(
    "https://statbank.cso.ie/StatbankServices/",
    "StatbankServices.svc/jsonservice/TitledetailsList"
  )

  if (cache) {
    data <- R.cache::loadCache(list("cso_toc", Sys.Date()), dirs = "csodata")
    if (!is.null(data)) {
      message("Loaded cached toc\n")
      return(data)
    } else {
      tbl <- data.frame(jsonlite::fromJSON(url))

      tbl2 <- tbl[c("id", "LastModified", "title")]
      tbl3 <- dplyr::mutate_if(tbl2, is.factor, as.character)
      tbl3$LastModified <- as.POSIXct(tbl3$LastModified,
                                      format = "%b %d %Y %I:%M%p", tz = "GMT")
      R.cache::saveCache(tbl3,
                         key = list("cso_toc", Sys.Date()), dirs = "csodata"
      )
      return(tbl3)
    }
  }
  # No caching --------------------------
  tbl <- data.frame(jsonlite::fromJSON(url))

  tbl2 <- tbl[c("id", "LastModified", "title")]
  tbl3 <- dplyr::mutate_if(tbl2, is.factor, as.character)

  tbl3$LastModified <- as.POSIXct(tbl3$LastModified,
                                  format = "%b %d %Y %I:%M%p", tz = "GMT")
  return(tbl3)
}


#' Search list of all table descriptions for given string
#'
#' Searches the list of all table descriptions returned by cso_get_toc() for a
#' given substring.
#'
#' @param string string. The text to search for. Case insensitive.
#' @param toc data.frame. The table of contents as returned by cso_get_toc. If
#' not given, will be re-downloaded (or retrieved from cache) using
#' cso_get_toc().
#' @return data frame of three character columns, with layout identical to
#' that of cso_get_toc. A subset of the results of cso_get_toc, with only rows
#' where the description field contains the entered string.
#'
#' @export
#' @examples
#' trv <- cso_search_toc("travel")
cso_search_toc <- function(string, toc = cso_get_toc()) {
  # Search string -----------------------
  pattern <- toupper(string)
  x <- toupper(toc$title)

  # Use grep to search ------------------
  toc[grep(pattern, x), ]
}
