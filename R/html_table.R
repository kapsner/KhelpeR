#' @title Helper function to render scrollable HTML tables
#'
#' @param table A table object to render.
#' @param merge_rows A character vector. The column names, of which cells with
#'   equal values should be merged. Predefined keys exist: "binary_results" for
#'   the output of `binary_results`.
#' @param linebreak A logical. Converts linebreaks to "<br>" (default: TRUE).
#'
#' @export
#'
html_table <- function(table, merge_rows = NULL, linebreak = TRUE) {

  stopifnot(
    is.logical(linebreak)
  )

  if (linebreak) {
    vec <- colnames(table)
    table <- table[
      , (vec) := lapply(.SD, function(x) {
        gsub("\\n", "<br>", x)
      }), .SDcols = vec
    ]
  }

  ret <- knitr::kable(table, format = "html", escape = FALSE)

  if (!is.null(merge_rows)) {
    if (merge_rows == "binary_results") {
      j <- which(colnames(table) %in%
                   c("Name", "Homoscedasticity",
                     "T-Test", "Wilcoxon-Test"))
    } else {
      j <- which(colnames(table) %in% merge_rows)
    }
    ret <- kableExtra::collapse_rows(
      kable_input = ret,
      columns = j,
      valign = "top"
    )
  }

  ret <- kableExtra::kable_styling(
    kable_input = ret,
    bootstrap_options = "bordered",
    full_width = F) %>%
    kableExtra::scroll_box(width = "100%")
  return(ret)
}


#' @title Helper function to render scrollable HTML tables using the
#'   flextables R package.
#'
#' @param theme A character. Set a specific flextable theme.
#' @param autofit A boolean. Whether or not to use flextable's `autofit`.
#'
#' @inheritParams html_table
#'
#' @export
#'
html_table_flex <- function(table,
                            merge_rows = NULL,
                            theme = NULL,
                            autofit = FALSE) {
  stopifnot(
    is.logical(autofit)
  )
  ret <- flextable::flextable(table) %>%
    flextable::fontsize(size = 9, part = "all")
  if (autofit) {
    ret <- flextable::autofit(ret)
  } else {
    # try to find optimal widhts by looking at the headers only
    widths <- flextable::dim_pretty(ret, part = "header")$widths
    ret <- flextable::width(
      x = ret,
      width = widths * 1.5
    )
  }
  if (!is.null(merge_rows)) {
    if (merge_rows == "binary_results") {
      j <- c("Name", "Homoscedasticity",
             "T-Test", "Wilcoxon-Test")
    } else {
      j <- merge_rows
    }
    ret <- flextable::merge_v(
      x = ret,
      j = j
    )
  }
  if (!is.null(theme)) {
    if (theme == "vanilla") {
      ret <- flextable::theme_vanilla(ret)
    }
  }
  return(ret)
}
