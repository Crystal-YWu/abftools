MultiPlot_CollectCh <- function(abf_list, intv_list = NULL, curs_list = NULL, ...) {

  plist = list()
  for (i in seq_along(abf_list)) {
    plist[[i]] <- CollectCh(abf = abf_list[[i]],
                            intv = intv_list[[i]],
                            curs = curs_list[[i]], ...)
  }

}
