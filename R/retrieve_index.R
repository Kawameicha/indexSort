#' Retrieve Index Sorted Data for a Given Cell Sorter
#'
#' This function retrieves index sorted data from various cell sorters.
#'
#' @param data A index sorted .fcs file
#' @param sorter A given cell sorter
#' @param ... Extra arguments, not used
#'
#' @return NULL
#'
#' @examples
#'
#' @export result

retrieve_index <- function(data, sorter = "aria", ...) {
  
  if (sorter %in% c("aria", "symphony")) { 
    
    # 1.1. Generate list 
    indSor <- rev(grep("INDEX SORTING LOCATIONS", names(data@description))) %>% 
      sapply(., function(x) unlist(data@description[[x]])) %>% 
      paste0 %>% 
      str_split(";") %>% 
      unlist 
    
    # 1.2. Generate result 
    result <- data.frame(IdxRow = as.integer(gsub("([0-9]?[0-9]),([0-9]?[0-9])", "\\1", indSor[indSor != ""])), 
                         IdxCol = as.integer(gsub("([0-9]?[0-9]),([0-9]?[0-9])", "\\2", indSor[indSor != ""]))) %>% 
      mutate(IdxRow, IdxRow = rawToChar(as.raw(65 + as.integer(.[, "IdxRow"])), multiple = TRUE)) %>% 
      mutate(IdxCol, IdxCol = formatC((IdxCol + 1), width = 2, flag = 0)) %>% 
      cbind(., data.frame(inputFCS@exprs)) 
    
    } else if (sorter %in% c("influx", "jazz")) { 
      
      # 2.1. Generate list
      indSor <- rev(grep("INDEXSORTPOSITIONS", names(inputFCS@description))) %>%
        sapply(., function(x) unlist(inputFCS@description[[x]])) %>%
        paste0 %>%
        str_split(",") %>%
        unlist %>%
        matrix(ncol = 3, byrow = TRUE) %>% 
        data.frame(Well   = .[, 1],
                   Tray.X = as.numeric(.[, 2]),
                   Tray.Y = as.numeric(.[, 3])) %>% 
        select(-starts_with("X")) %>% 
        mutate_if(is.numeric, function(x) round(x/100)) %>% 
        mutate(Sort.Result.Bits = 32769)
      
      # 2.3. Generate result 
      result <- data.frame(inputFCS@exprs) %>%
        mutate_at(vars(starts_with("Tray")), function(x) round(x/100)) %>% 
        left_join(., indSor, all.x = TRUE) %>% 
        mutate(IdxRow = gsub("([[:alpha:]])([0-9]?[0-9])", "\\1", Well),
               IdxCol = gsub("([[:alpha:]])([0-9]?[0-9])", "\\2", Well)) %>% 
        mutate(IdxCol, IdxCol = formatC((as.numeric(IdxCol)), width = 2, flag = 0))
      
    } 
  
  return(result) 
  
  }
