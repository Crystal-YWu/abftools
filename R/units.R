parse_unit_prefix <- function(unit) {

  long_prefix_regex <- "^(pico|nano|micro|milli|kilo|mega|Giga|Tera)"
  grepl(long_prefix_regex, unit)
}

parse_unit_suffix <- function(unit) {

  n <- nchar(unit)
  long_prefix <- parse_unit_prefix(unit)

  if (long_prefix) {
    idx <- switch(substr(unit, 1, 4),
                  micr = ,
                  mill = 6,
                  pico = ,
                  nano = ,
                  kilo = ,
                  mega = ,
                  Giga = ,
                  Tera = 5,
                  1)
  } else {
    if (startsWith(unit, "mol")) {
      idx <- 1L
    } else {
      idx <- switch(substr(unit, 1, 1),
                    µ = ,
                    u = ,
                    m = ,
                    p = ,
                    n = ,
                    k = ,
                    M = ,
                    G = ,
                    T = 2,
                    1)
    }
  }

  substr(unit, idx, n)
}

parse_unit_scale <- function(unit) {

  long_prefix <- parse_unit_prefix(unit)
  if (long_prefix) {
    ans <- switch(substr(unit, 1, 4),
                  pico = 1e-12,
                  nano = 1e-9,
                  micr = 1e-6,
                  mill = 1e-3,
                  kilo = 1e3,
                  mega = 1e6,
                  Giga = 1e9,
                  Tera = 1e12,
                  1)
  } else {
    if (startsWith(unit, "mol")) {
      ans <- 1
    } else {
      ans <- switch(substr(unit, 1, 1),
                    p = 1e-12,
                    n = 1e-9,
                    µ = ,
                    u = 1e-6,
                    m = 1e-3,
                    k = 1e3,
                    M = 1e6,
                    G = 1e9,
                    T = 1e12,
                    1)
    }
  }

  ans
}

#' Basic scaling of units.
#'
#' Scale x of unit to a new unit with different prefix
#'
#' @param x a numeric vector.
#' @param unit unit of x.
#' @param scale new scale of prefix
#'
#' @return a numeric vector
#' @export
#'
scale_unit <- function(x, unit, scale = c("1", "pico", "nano", "micro", "milli",
                                         "kilo", "mega", "Giga", "Tera")) {

  scale <- as.character(scale)
  scale <- match.arg(scale)

  long_prefix <- parse_unit_prefix(unit)
  suffix <- parse_unit_suffix(unit)

  if (long_prefix) {
    if (scale == "1") {
      prefix <- ""
    } else {
      prefix <- scale
    }
  } else {
    prefix <- switch(scale,
                     pico  = "p", nano  = "n", micro = "µ", milli = "m",
                     kilo  = "k", mega  = "M", Giga  = "G", Tera  = "T", "")
  }
  new_unit <- paste0(prefix, suffix)

  old_scale <- parse_unit_scale(unit)
  new_scale <- parse_unit_scale(scale)
  scale_factor <- old_scale / new_scale
  new_x <- x * scale_factor

  list(
    x = new_x,
    unit = new_unit
  )
}
