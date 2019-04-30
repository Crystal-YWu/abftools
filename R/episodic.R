#This function applies episodic values in given by the intv_list to function f
#and returns a matrix of the results of f. f should accept na.rm
Episodic_colFunc <- function(abf_list, intv_list, channel, colFunc, ...) {

  n <- length(abf_list)
  nepi <- max(unlist(nEpi(abf_list)))

  m <- matrix(NA, nrow = n, ncol = nepi)
  colnames(m) <- DefaultEpiLabel(nepi)
  rownames(m) <- unlist(GetTitle(abf_list))

  colFunc <- match.fun(colFunc)

  for (i in seq_len(n)) {
    if (any(is.na(intv_list[[i]]))) {
      next
    } else {
      if (is.null(intv_list[[i]])) {
        ret <- colFunc(abf_list[[i]][,, channel], ...)
      } else {
        ret <- colFunc(abf_list[[i]][MaskIntv(intv_list[[i]]), , channel], ...)
      }
      epi <- GetAvailEpisodes(abf_list[[i]])
      m[i, epi] <- ret[epi]
    }
  }

  m
}

apply_colFunc_episode <- function(abf_, intv_, channel_, colFunc_, ...) {

  colFunc_ <- match.fun(colFunc_)

  d <- dim(abf_)

  if (any(is.na(intv_))) {
    rep(NA, d[2])
  } else {
    if (is.null(intv_)) {
      colFunc_(abf_[,, channel_], ...)
    } else {
      colFunc_(abf_[MaskIntv(intv_),, channel_], ...)
    }
  }
}


