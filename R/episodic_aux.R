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
  colnames(m) <- DefaultEpiLabel(nepi)
  rnames <- c()
  for (i in seq_along(abf_list)) {
    rnames[i] <- GetTitle(abf_list[[i]])
  }
  rownames(m) <- rnames

  for (i in seq_along(abf_list)) {
    if (any(is.na(intv_list[[i]]))) {
      next
    } else {
      if (is.null(intv_list[[i]])) {
        mask <- seq_len(nEpi(abf_list[[i]]))
      } else {
        mask <- MaskIntv(intv_list[[i]])
      }
      ret <- f(abf_list[[i]][mask, , channel])
      epi <- GetAvailEpisodes(abf_list[[i]])
      for (j in epi)
        m[i, j] <- ret[j]
    }
  }

  return(m)
}
