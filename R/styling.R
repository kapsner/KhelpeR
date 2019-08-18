#' @title kableTable
#'
#' @param dataset A data.table object.
#'
#' @export
#'
kableTable <- function(dataset){
  knitr::kable(dataset) %>%
    kableExtra::kable_styling(bootstrap_options = "bordered", full_width = F)
}
