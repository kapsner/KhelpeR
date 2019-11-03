#' @title kable_table
#'
#' @param dataset A data.table object.
#'
#' @export
#'
kable_table <- function(dataset) {

  knitr::kable(dataset) %>%
    kableExtra::kable_styling(
      bootstrap_options = "bordered",
      full_width = F
    )
}
