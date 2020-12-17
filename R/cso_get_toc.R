#' Returns a data frame with all valid CSO PxStat tables listed sequentially
#' by id number, e.g. A0101, A0102, A0103, etc.
#'
#' Checks the CSO PxStat API for a list of all the table codes (e.g. A0101,
#' A0102, A0103, etc.), which also includes date last modified and title for
#' each table, and returns this list as an R data frame.
#'
#' The data is pulled from the ReadCollection on the CSO API. See
#' \url{https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadCollection}
#' for more information on this.
#'
#' @param cache logical. If TRUE (default) the table of contents is cached
#' with the system date as a key.
#' @param suppress_messages logical. If FALSE (default) a message is printed
#' when loading a previously cached table of contents.
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
cso_get_toc <- function(cache = TRUE, suppress_messages = FALSE) {
  url <- paste0(
    
    "https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadCollection"
    
  )
  
  # cache
  if (cache) {
    data <- R.cache::loadCache(list("cso_toc", Sys.Date()), dirs = "csodata")
    if (!is.null(data)) {
      if (!suppress_messages) {
        message("Loaded cached toc\n")
      }
      return(data)
    } else {
      # Check for errors using trycatch since PxStat API does not support
      # html head requests.
      error_message =  paste0("Failed retrieving table of contents. Please ",
                              "check internet connection and that data.cso.ie is online")
      
      tbl <- tryCatch({
        data.frame(jsonlite::fromJSON(url))
        
      }, warning = function(w) {
        print(paste0("Warning: ", error_message))
        return(NULL)
      }, error = function(e) {
        print(paste0("Error: ", error_message))
        return(NULL)
      })
      
      
      tbl2 <- cbind(tbl[c("link.item.updated","link.item.label")],data.frame(tbl$link.item.extension$matrix))
      tbl3 <- dplyr::mutate_if(tbl2, is.factor, as.character)
      
      names(tbl3)[1] <- "LastModified"
      names(tbl3)[2] <- "title"
      names(tbl3)[3] <- "id"
      
      tbl3$LastModified <- as.POSIXct(tbl3$LastModified,
                                      format = "%M", tz = "GMT")
      R.cache::saveCache(tbl3,
                         key = list("cso_toc", Sys.Date()), dirs = "csodata"
      )
      return(tbl3)
    }
  }
  # No caching --------------------------
  tbl <- data.frame(jsonlite::fromJSON(url))
  tbl2 <- cbind(tbl[c("link.item.updated","link.item.label")],data.frame(tbl$link.item.extension$matrix))
  tbl3 <- dplyr::mutate_if(tbl2, is.factor, as.character)
  
  
  names(tbl3)[1] <- "LastModified"
  names(tbl3)[2] <- "title"
  names(tbl3)[3] <- "id"
  
  tbl3$LastModified <- as.POSIXct(tbl3$LastModified,
                                  format = "%M", tz = "GMT")
  
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
cso_search_toc <- function(string, toc = cso_get_toc(suppress_messages = TRUE)) {
  # Error Checking ----------------------
  if (is.null(toc)) {
    return(NULL)
  }

  # Search string -----------------------
  pattern <- toupper(string)
  x <- toupper(toc$title)

  # Use grep to search ------------------
  toc[grep(pattern, x), ]
}
