#' Plot a Plate Visual from Index Sorted Data
#'
#' This function plots a plate visual from index sorted data.
#'
#' @param data A index sorted .fcs file
#' @param dim A plate's dimension c(x, y)
#' @param ... Extra arguments, not used
#'
#' @return NULL
#'
#' @examples
#'
#' @export result

explore_plate <- function(data, dim = c(12, 8), ...) {
  
  # 1. Plate template
  plaTem <- data %>% select(IdxRow, IdxCol)
  
  # 1. Plate visual
  result <- data %>%
    ggplot(aes(y = IdxRow, x = as.integer(IdxCol))) + 
    geom_point(shape = 1, size = 12, data = plaTem) +
    geom_point(aes(colour = nbrCell), size = 10, alpha = .75) +
    geom_text(aes(label = nbrCell, alpha = .25)) +
    scale_y_discrete(limits = rev(LETTERS[1:dim[2]])) +
    scale_x_continuous(breaks = seq(1, dim[1])) +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    labs(x = NULL, y = NULL,
         title = "Number of Cells per Well",
         caption = "indexSort v.0.1.0")
  
  return(result)
}
