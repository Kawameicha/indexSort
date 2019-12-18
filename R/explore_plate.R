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
#' explore_plate(result, vars = c("APC.A", "FITC.A", "Pacific.Blue.A"))
#' 
#' # Remove cell number values
#' explore_plate(result, vars = c("APC.A", "FITC.A", "Pacific.Blue.A"), cell = FALSE)
#' 
#' # Change plate dimension
#' explore_plate(result, vars = c("APC.A", "FITC.A", "Pacific.Blue.A"), dim = c(16, 24))
#' 
#' # Remove the legend
#' explore_plate(result, vars = c("APC.A", "FITC.A", "Pacific.Blue.A"), legend = FALSE)
#' 
#' # Adapt wells' size
#' explore_plate(result, vars = c("APC.A", "FITC.A", "Pacific.Blue.A"), circle = 8)
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
    labs(title   = "Median Fluorescence per Well",
         caption = "indexSort v.0.1.3") +
    facet_wrap(~key, ncol = 2)
  
  return(result)
}
