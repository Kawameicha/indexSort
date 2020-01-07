#' Values of 96 cells.
#'
#' A flowFrame containing the values of 96 single-cells and
#' location keywords.
#'
#' @format A flowFrame with 96 rows and 8 variables:
#' \describe{
#'   \item{FSC}{forward scatter}
#'   \item{SSC}{side scatter}
#'   \item{APC}{allophycocyanin}
#'   \item{FITC}{fluorescein}
#'   \item{PerCP}{peridinin-chlorophyll}
#'   \item{PI}{propidium iodide}
#'   \item{$`INDEX SORTING LOCATIONS_x`}{location keyword}
#'   \item{$`$CYT`}{cytometer keyword}
#'   ...
#' }
#' @source Handcrafted, personal work
"inputARIA"

#' Indexed values of 96 cells.
#'
#' A dataset containing the indexed values of 96 single-cells.
#'
#' @format A data frame with 96 rows and 8 variables:
#' \describe{
#'   \item{IdxRow}{indexed row}
#'   \item{IdxCol}{indexed column}
#'   \item{FSC}{forward scatter}
#'   \item{SSC}{side scatter}
#'   \item{APC}{allophycocyanin}
#'   \item{FITC}{fluorescein}
#'   \item{PerCP}{peridinin-chlorophyll}
#'   \item{PI}{propidium iodide}
#'   ...
#' }
#' @source Handcrafted, personal work
"indexARIA"