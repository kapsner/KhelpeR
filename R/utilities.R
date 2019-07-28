# define %notin% function
"%!in%" <- function(x,y){!("%in%"(x,y))}

# define getTimestamp function
getTimestamp <- function(mode){
  if (mode=="human"){
    time <- paste(substr(Sys.time(), 1, 10), substr(Sys.time(), 12, 16))
  }
  if (mode=="machine"){
    time <- paste(gsub("\\-", "", substr(Sys.time(), 1, 10)), gsub("\\:", "", substr(Sys.time(), 12, 17)), sep="_")
  }
  return(time)
}

# mark p-values with asterixes
pMarker <- function(value){
  ret <- tryCatch({
    suppressWarnings(value <- as.numeric(as.character(value)))
    if (value < 0.001){
      out <- paste0(rep("\u002A", 3), collapse = "")
    } else if (value < 0.01){
      out <- paste0(rep("\u002A", 2), collapse = "")
    } else if (value < 0.05){
      out <- "\u002A"
    } else if (value < 0.1){
      out <- "\u00B0"
    } else {
      out <- ""
    }
  }, error = function(e){
    out <- ""
  }, warning = function(e){
    out <- ""
  }, finally = function(f){
    return(out)
  })
  return(ret)
}
