onset <- function(abf, channel, ref) {

  f <- function(x) x >= ref

  v <- mapnd(abf[,, channel], f, along = 2L)

  unlist(reduce_along(v, 1L, function(x) which(x)[1]))
}
