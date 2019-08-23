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
        mutate_at(vars(starts_with("Tray")), function(x) round(x / 100)) %>% 
        left_join(., indSor, all.x = TRUE) %>% 
        mutate(IdxRow = gsub("([[:alpha:]])([0-9]?[0-9])", "\\1", Well),
               IdxCol = gsub("([[:alpha:]])([0-9]?[0-9])", "\\2", Well)) %>% 
        mutate(IdxCol, IdxCol = formatC((as.numeric(IdxCol)), width = 2, flag = 0)) %>% 
        select(IdxRow, IdxCol, everything(), -Well)
      
    } else if (sorter %in% c("astrios")) {
      
      # Generate results
      result <- data.frame(inputFCS@exprs) %>% 
        mutate(bits   = lapply(Sort.Classifier, function(x) as.numeric(intToBits(x))),
               IdxRow = sapply(bits, function(x) sum(x[27], x[28]*2, x[29]*4 + x[30]*8 + x[31]*16 + x[32]*32)),
               IdxCol = sapply(bits, function(x) sum(x[21], x[22]*2, x[23]*4 + x[24]*8 + x[25]*16 + x[26]*32))) %>%
        mutate(IdxRow = rawToChar(as.raw(64 + as.integer(.[, "IdxRow"])), multiple = TRUE)) %>%
        mutate(IdxCol = formatC((IdxCol), width = 2, flag = 0)) %>%
        mutate(IdxRow = ifelse(IdxRow == "@", NA_real_,  IdxRow)) %>%
        mutate(IdxCol = ifelse(IdxCol == "00", NA_real_,  IdxCol)) %>% 
        select(IdxRow, IdxCol, everything(), -bits)
      
    }
  
  return(result) 
  
  }
