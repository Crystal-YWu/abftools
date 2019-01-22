ParseDataFrameIV <- function(df, has_id = FALSE) {

  dict <- list()
  if (has_id) {
    dict$id <- "id"
  }
  dict$Voltage <- c("voltage", "v")
  dict$Current <- c("current", "i")
  dict$SEMC <- c("semc", "sem c", "semi", "sem i")
  dict$SEMV <- c("semv", "sem v")

  ExtractDataFrame(df, dict)
}

#' Extract from a data.frame
#'
#' Extract columns from a data.frame by matching colnames from a named list. See
#' ParseDataFrameIV for example usage.
#'
#' @param df data.frame
#' @param dict named list
#' @param match_func a functio to match
#' @param lower_case whether to lower cases of colnames
#'
#' @return a data.frame
#' @keywords internal
ExtractDataFrame <- function(df, dict, match_func = startsWith, lower_case = TRUE) {

  parsed <- list()

  cnames <- colnames(df)
  if (lower_case) {
    cnames <- tolower(cnames)
  }
  dnames <- names(dict)

  n <- length(cnames)

  for (i in seq_along(dict)) {

    to_match_list <- dict[[i]]
    matched <- rep(FALSE, n)
    for (to_match in to_match_list) {
      matched <- matched | match_func(cnames, to_match)
    }

    var_name <- dnames[i]
    matched <- which(matched)
    if (length(matched)) {
      matched_msg <- paste0("Matched columns: ", toString(cnames[matched]))
      matched <- FirstElement(matched, matched_msg)
      parsed[[var_name]] <- df[, matched]
    } else {
      parsed[[var_name]] <- NA
    }

  }

  data.frame(parsed)
}

EnforceListNames <- function(x) {

  n <- length(x)
  xnames <- names(x)

  default_names <- paste0("item", seq_len(n))

  if (is.null(xnames)) {
    names(x) <- default_names
  } else {
    replace_names <- ifelse(xnames == "", default_names, xnames)
    names(x) <- replace_names
  }

  x
}

#Now a bit faster. But still much slower than rbindlist
#However I would prefer less dependency.
BindDataFrameList <- function(x) {

  id <- NULL
  for (idx in names(x)) {
    n <- nrow(x[[idx]])
    id <- c(id, rep(idx, n))
  }

  df <- do.call(rbind, x)

  cbind(id, df)
}

#' Melt channel data of abf objects.
#'
#' @param abf an abf/list of abf objects.
#' @param channel channel/channels to melt.
#' @param intv OPTIONAL, an interval to process.
#' @param map_func a mapping function to process data.
#' @param abf_id_func OPTIONAL, a function accepts an abf object and returns an identifier of the objects.
#' @param epi_id_func OPTIONAL, a function accepts an abf object and returns a vector of identifiers of all episodes.
#' @param chan_id_func OPTIONAL, a function accepts an abf object and returns a vectors of identifiers of all channels.
#' @param na.intv.rm whether to remove NA intervals passed.
#' @param ... further arguments passed to map_func.
#'
#' @return a melted data frame
#' @export
#'
MeltAbfChannel <- function(abf, channel, intv = NULL,
                           map_func = mean,
                           abf_id_func = GetTitle,
                           epi_id_func = DefaultEpiLabel,
                           chan_id_func = DefaultChanLabel,
                           na.intv.rm = TRUE, ...) {

  #convert abf, intv to lists and check lengths
  if (IsAbf(abf)) {
    abf <- list(abf)
  } else if (!IsAbfList(abf)) {
    err_class_abf_list()
  }
  #check channel
  if (missing(channel) || is.null(channel)) {
    #abf is now a list of abf instead, so we use abf[[1]] instead
    channel <- GetAllChannels(abf[[1]])
  }
  channel <- unlist(channel)
  for (tmp in abf) {
    if (!AssertChannel(tmp, channel)) {
      err_channel()
    }
  }

  intv <- ExpandList(intv, abf)
  if (is.null(intv)) {
    err_assert_len(intv, abf)
  }

  #check channel ids
  if (is.function(chan_id_func)) {
    channel_id <- chan_id_func(abf[[1]])[channel]
    for (tmp in abf) {
      tmp <- chan_id_func(tmp)[channel]
      if (!all(channel_id == tmp)) {
        err_chan_id()
      }
    }
  }

  f <- WrapMappingFunc(map_func, channel = channel, abf_id_func = abf_id_func,
                       epi_id_func = epi_id_func, chan_id_func = chan_id_func,
                       ret.df = TRUE, ...)
  melted <- NULL
  for (i in seq_along(abf)) {
    if (na.intv.rm && any(is.na(intv[[i]]))) {
      #exclude NA intv
      next
    }
    tmp <- f(abf[[i]], intv[[i]])
    melted <- rbind(melted, tmp)
  }

  return(melted)
}
