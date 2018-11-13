MultiPlotP <- function(abf_list, channel, colour, time_unit,
                       num_label, title_label, unify_y, ...) {

  p <- list()
  if (unify_y) {
    ylimit <- UniYLim(abf_list, channel)
    for (i in seq_along(abf_list))
      p[[i]] <- PlotChannel(abf_list[[i]], channel, colour, time_unit, ...) + ylim(ylimit)
  } else {
    for (i in seq_along(abf_list))
      p[[i]] <- PlotChannel(abf_list[[i]], channel, colour, time_unit, ...)
  }

  has_title <- num_label || title_label
  if (has_title) {
    for (i in seq_along(abf_list)) {
      title_s <- paste0(ifelse(num_label, paste0(i, ". "), ""),
                        ifelse(title_label, GetTitle(abf_list[[i]]), ""))
      p[[i]] <- p[[i]] + ggtitle(title_s)
    }
  }

  return(p)
}
