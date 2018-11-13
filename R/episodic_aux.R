#This function applies episodic values in given by the intv_list to function f
#and returns a matrix of the results of f. f should accept na.rm
EpisodicInterval_f <- function(abf_list, intv_list, channel, f) {

  n <- length(abf_list)
  nepi <- 0L
  #nepi of every abf object in an abf_list is supposed to be the same
  #In case of different sizes, we use the largest nepi as ncol
  for (i in seq_along(abf_list)) {
    nepi <- max(nepi, nEpi(abf_list[[i]]))
  }
  m <- matrix(NA, nrow = n, ncol = nepi)
  colnames(m) <- paste0("epi", seq_len(nepi))

  for (i in seq_along(abf_list)) {
    if (any(is.na(intv_list[[i]]))) {
      next
    } else {
      mask <- seq(intv_list[[i]][1], intv_list[[i]][2])
      #Do not use AvailEpisode to extract channel info, so we always have correct
      #episodic order
      ret <- f(abf_list[[i]][mask, , channel])
      #Results are padded to left
      nepi <- nEpi(abf_list[[i]])
      for (j in seq_len(nepi))
        m[i, j] <- ret[j]
    }
  }

  return(m)
}
