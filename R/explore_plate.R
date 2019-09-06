#' Plot a Plate Visual from Index Sorted Data
#'
#' This function plots a plate visual from index sorted data.
#'
#' @param data A index sorted .fcs file
#' @param vars Features to be used
#' @param cell Display cell number values
#' @param dim A plate's dimension c(x, y)
#' @param ... Extra arguments, not used
#'
#' @return NULL
#'
#' @examples
#'
#' @export result

explore_plate <- function(data, vars = c("IdxRow", "IdxCol"), cell = TRUE, dim = c(8, 12), ...) {
  
  # 1. Plate template
  plaTem <- data %>% select(IdxRow, IdxCol)
  
  # 2. Cell number
  cellNb <- data %>%
    select(IdxRow, IdxCol) %>% 
    group_by(IdxRow, IdxCol) %>% 
    tally %>% 
    ungroup
  
  # 3. Plate visual
  result <- data %>%
    select(IdxRow, IdxCol, vars) %>%
    left_join(., cellNb) %>% 
    group_by(IdxRow, IdxCol) %>% 
    summarise_if(is.numeric, median) %>% 
    gather(key = "key", value = "value", -IdxRow, -IdxCol, -n) %>% 
    ggplot(aes(y = IdxRow, x = as.integer(IdxCol))) + 
    geom_point(shape = 1, size = 12, data = plaTem) +
    geom_point(aes(colour = log2(value)), size = 10, alpha = .75) +
    {if (cell == TRUE) geom_text(aes(label = n, alpha = .25))} +
    scale_y_discrete(limits = rev(LETTERS[1:dim[1]])) +
    scale_x_continuous(breaks = seq(1, dim[2])) +
    scale_colour_gradient(low = "#8acf81", high = "#ef5534") +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid.minor = element_blank(),
          strip.text = element_text(size = 12),
          strip.background = element_blank()) +
    labs(x = element_blank(), 
         y = element_blank(),
         title = "Median Fluorescence per Well",
         caption = "indexSort v.0.1.2") +
    facet_wrap(~key, ncol = 2)
  
  return(result)
}
