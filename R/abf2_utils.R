colSds <- function(df, na.rm = FALSE) {

  n <- ifelse(na.rm, colSums(!is.na(df)), nrow(df))
  #Custom implementation for the missing colSds, may not be most effective
  ret <- colMeans(df * df, na.rm = na.rm) - colMeans(df, na.rm = na.rm)^2
  ret <- sqrt(ret * n / (n - 1))

  return(ret)
}

colSems <- function(df, na.rm = FALSE) {

  sds <- colSds(df, na.rm)
  sqn <- sqrt(length(sds))

  return(sds / sqn)
}

#abf_ylab <- function(abf, channel) paste(abf$ChannelNamePlot[channel], "/", abf$ChannelUnit[channel])
