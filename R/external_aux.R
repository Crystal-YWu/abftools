#Generalise external algorithm calls
#Principles:
#External algorithms are used to analyse data in an abf object, and should be
#transparent to meta info.
#Function calls process episodic time-series y.
#ExternalAlgoEpoch returns a list of vectors, since epoch length can vary in
#different episodes.
#ExternalAlgoIntv returns named column matrix for each selected episode, which
#can be more convenient for subsequent data analysis.

ExternalAlgoEpoch <- function(abf, epoch, episodes, channel, func, algo, ...) {

  if (episodes[1] == 0) {
    episodes <- seq.int(nEpi(abf))
  }
  if (is.character(epoch)) {
    epoch <- GetEpochId(epoch)
  }
  algo_f <- paste0(func, "_", algo)

  algo_ret <- list()
  for (i in episodes) {
    y <- ExtractFrom(abf, epoch, i, channel)
    algo_ret[[i]] <- do.call(algo_f, list(y = y, ...))
  }
  algo_names <- paste0("epi", seq.int(length(algo_ret)))
  names(algo_ret) <- algo_names

  return(algo_ret)
}

ExternalAlgoIntv <- function(abf, intv, episodes, channel, func, algo, ...) {

  if (episodes[1] == 0) {
    episodes <- seq.int(nEpi(abf))
  }
  algo_f <- paste0(func, "_", algo)
  mask <- MaskIntv(intv)

  idx <- 0L
  algo_ret <- matrix(0.0, nrow = intv[3], ncol = length(episodes))
  for (i in episodes) {
    idx <- idx + 1L
    y <- abf[mask, i, channel]
    algo_ret[, idx] <- do.call(algo_f, list(y = y, ...))
  }
  colnames(algo_ret) <- paste0("epi", episodes)

  return(algo_ret)
}
