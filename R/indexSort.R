#' @title Retrieve Index Sorted Data for a Given Cell Sorter
#'
#' @description The `retrieve_index` was designed in order to give  
#' researchers the ability to automatically gate wells from .fcs data  
#' generated using various cell sorters. It will generate parameters  
#' corresponding to the `x` and `y` axes (IdxRow and IdxCol).
#'
#' @param data An index sorted .fcs file
#' @param sorter Specify the cell sorter. Possible values are `aria`,  
#' `symphony`, `influx`, `jazz`, `astrios`.
#' @param ... Extra arguments, not used
#'
#' @return A `data.frame` object
#'
#' @examples
#' library(flowCore)
#' library(tidyverse)
#' 
#' # Retrieve indexed data from an Aria
#' retrieve_index(inputARIA)
#' 
#' # Retrieve data from specific sorter
#' result <- retrieve_index(inputARIA, sorter = "aria")
#' 
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom stats median
#' @importFrom stringr str_split
#' @importFrom tidyr pivot_longer
#'
#' @export

retrieve_index <- function(data,
                           sorter = "aria", ...) {
  
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
      bind_cols(., data.frame(data@exprs)) 
    
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
      
      # 2.2. Generate result 
      result <- data.frame(inputFCS@exprs) %>%
        mutate_at(vars(starts_with("Tray")), function(x) round(x / 100)) %>% 
        left_join(., indSor, all.x = TRUE) %>% 
        mutate(IdxRow = gsub("([[:alpha:]])([0-9]?[0-9])", "\\1", Well),
               IdxCol = gsub("([[:alpha:]])([0-9]?[0-9])", "\\2", Well)) %>% 
        mutate(IdxCol, IdxCol = formatC((as.numeric(IdxCol)), width = 2, flag = 0)) %>% 
        select(IdxRow, IdxCol, everything(), -Well)
      
    } else if (sorter %in% c("astrios")) {
      
      # 3.1. Generate result
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

#' @title Plot a Plate Visual from Index Sorted Data
#'
#' @description The `explore_plate` is used to generate a set of illustrations 
#' that automatically display the Median Fluorescent Intensity (MFI) per well for 
#' the selected parameters. Additionally, the number of cells will be displayed 
#' for each well.
#'
#' @param data An indexed `data.frame` or `tibble` object
#' @param vars Features to be displayed as MFI
#' @param cell Display or not cell number values
#' @param dim A plate's dimension c(x, y)
#' @param legend Display or not fluorescence legend
#' @param circle Size of the wells on illustration
#' @param ... Extra arguments, not used
#'
#' @return A `ggplot` object
#'
#' @examples 
#' library(tidyverse)
#' 
#' explore_plate(indexARIA, vars = c("APC", "FITC", "PerCP", "PI"))
#' 
#' # Remove the legend
#' explore_plate(indexARIA, vars = c("APC", "FITC", "PerCP", "PI"), legend = FALSE)
#'
#' @export 

explore_plate <- function(data,
                          vars   = c("IdxRow", "IdxCol"),
                          cell   = TRUE,
                          dim    = c(8, 12),
                          legend = TRUE,
                          circle = 12, ...) {
  
  # 1. Plate template
  plaTem <- data %>%
    select(IdxRow, IdxCol)
  
  # 2. Cell number
  cellNb <- data %>%
    select(IdxRow, IdxCol) %>% 
    group_by(IdxRow, IdxCol) %>% 
    tally %>% 
    ungroup
  
  # 3. Plate visual
  result <- data %>%
    select(IdxRow, IdxCol, vars) %>%
    left_join(., cellNb, by = c("IdxRow", "IdxCol")) %>% 
    group_by(IdxRow, IdxCol) %>% 
    summarise_if(is.numeric, median) %>% 
    pivot_longer(c(-IdxRow, -IdxCol, -n), names_to = "key", values_to = "value") %>% 
    ggplot(aes(y = IdxRow, x = as.integer(IdxCol))) + 
    geom_point(shape = 1, size = circle, data = plaTem) +
    geom_point(aes(colour = value), size = .8 * circle, alpha = .75) +
    {if (cell == TRUE) geom_text(aes(label = n, alpha = .25))} +
    {if (cell == TRUE) guides(alpha = FALSE)} +
    scale_y_discrete(limits = rev(LETTERS[1:dim[1]])) +
    scale_x_continuous(breaks = seq(1, dim[2])) +
    scale_colour_gradient(low   = "#8acf81",
                          high  = "#ef5534",
                          trans = "log2",
                          name  = element_blank()) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(size = 12),
          strip.background = element_blank(),
          axis.title       = element_blank()) +
    {if (legend == FALSE) theme(legend.position = "none")} +
    labs(title = "Median Fluorescence per Well") +
    facet_wrap(~key, ncol = 2)
  
  return(result)
}
