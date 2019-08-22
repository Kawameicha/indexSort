#' Retrieve Index Sorted Data for a Given Cell Sorter
#'
#' This function retrieves index sorted data from various cell sorter.
#'
#' @param data A index sorted .fcs file
#' @param sorter A cell sorter, not used
#' @param ... Extra arguments, not used
#'
#' @return NULL
#'
#' @examples
#'
#' @export result

retrieve_index <- function(data, sorter, ...) {
  
  # 1. Preprocessing
  indSor <- rev(grep("INDEX SORTING LOCATIONS", names(data@description))) %>%
    sapply(., function(x) unlist(data@description[[x]])) %>%
    paste(collapse = "") %>%
    str_split(";") %>%
    unlist
  
  # 2. Generate result
  result <- data.frame(IdxRow = as.integer(gsub("([0-9]?[0-9]),([0-9]?[0-9])", "\\1", indSor[indSor != ""])),
                       IdxCol = as.integer(gsub("([0-9]?[0-9]),([0-9]?[0-9])", "\\2", indSor[indSor != ""]))) %>%
    mutate(IdxRow, IdxRow = rawToChar(as.raw(65 + as.integer(.[, "IdxRow"])), multiple = TRUE)) %>%
    mutate(IdxCol, IdxCol = formatC((IdxCol + 1), width = 2, flag = 0))
  
  return(result)
}
