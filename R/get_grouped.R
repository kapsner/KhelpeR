get_grouped_N <- function(lvls, dataset, variable, group_var) {
  ret <- sapply(
    X = lvls,
    FUN = function(x) {
      dataset[!is.na(get(variable)), ][get(group_var) == x, .N]
    },
    USE.NAMES = F
  )
  return(ret)
}

get_grouped_dispersion <-
  function(lvls, dataset, variable, group_var, digits) {
  ret <- sapply(
    X = lvls,
    FUN = function(x) {
      dispersion(
        vector = dataset[!is.na(get(variable)),
        ][
          get(group_var) == x, get(variable)
        ],
        digits = digits
      )
    },
    USE.NAMES = F
  )
  return(ret)
  }

get_grouped_normality <-
  function(lvls, dataset, variable, group_var, digits) {
  ret <- sapply(
    X = lvls,
    FUN = function(x) {
      shapiro_util(
        vector = dataset[!is.na(get(variable)),
        ][
          get(group_var) == x, get(variable)
        ],
        digits = digits
      )
    },
    USE.NAMES = F
  )
  return(ret)
}
