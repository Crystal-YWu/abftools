#This function applies episodic values in given by the intv_list to function f
#and returns a matrix of the results of f. f should accept na.rm
Episodic_ColFunc <- function(abf_list, intv_list, channel, f, ...) {

  n <- length(abf_list)
  nepi <- max(unlist(nEpi(abf_list)))

  m <- matrix(NA, nrow = n, ncol = nepi)
  colnames(m) <- DefaultEpiLabel(nepi)
  rownames(m) <- unlist(GetTitle(abf_list))

  for (i in seq_along(abf_list)) {
    if (any(is.na(intv_list[[i]]))) {
      next
    } else {
      if (is.null(intv_list[[i]])) {
        mask <- seq_len(nPts(abf_list[[i]]))
      } else {
        mask <- MaskIntv(intv_list[[i]])
      }
      ret <- f(abf_list[[i]][mask, , channel], ...)
      epi <- GetAvailEpisodes(abf_list[[i]])
      for (j in epi) m[i, j] <- ret[j]
    }
  }

  m
}

