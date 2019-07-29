# get list of cutoff values for outliers
getOutlierLimits <- function(vector){

  # OutLo: Number of outliers (records below Q25-1.5*IQR)
  # OutHi: Number of outliers (records above Q75+1.5*IQR)

  # get quantiles of vector
  Q <- stats::quantile(vector, probs=c(.25, .75), na.rm=T, names=F)

  # IQR
  I <- stats::IQR(vector, na.rm=T)

  # OutLo: < Q25-1.5*IQR
  OutLo <- Q[1]-1.5*I

  # OutHi: > Q75+1.5*IQR
  OutHi <- Q[2]+1.5*I

  return(list(lo=OutLo, hi=OutHi))
}


flagOutliers <- function(vector){
  # OutLo: Number of outliers (records below Q25-1.5*IQR)
  # OutHi: Number of outliers (records above Q75+1.5*IQR)
  
  # get quantiles of vector
  Q <- stats::quantile(vector, probs=c(.25, .75), na.rm=T, names=F)
  
  # IQR
  I <- stats::IQR(vector, na.rm=T)
  
  # OutLo: < Q25-1.5*IQR
  OutLo <- Q[1]-1.5*I
  
  # OutHi: > Q75+1.5*IQR
  OutHi <- Q[2]+1.5*I
  
  return(vector < OutLo | vector > OutHi)
}

# replace outliers
capOutliers <- function(vector){
  # OutLo: Number of outliers (records below Q25-1.5*IQR)
  # OutHi: Number of outliers (records above Q75+1.5*IQR)

  # get quantiles of vector
  Q <- stats::quantile(vector, probs=c(.25, .75), na.rm=T, names=F)

  # IQR
  I <- stats::IQR(vector, na.rm=T)

  # OutLo: < Q25-1.5*IQR
  OutLo <- Q[1]-1.5*I

  # OutHi: > Q75+1.5*IQR
  OutHi <- Q[2]+1.5*I

  # cap low outliers
  vector[which(vector < OutLo)] <- OutLo

  # cap high outliers
  vector[which(vector > OutHi)] <- OutHi

  # return vector
  return(vector)
}
