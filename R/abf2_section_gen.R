gen_struct_df <- function(struct_def) {

  n <- length(struct_def$field)
  mx <- matrix(NA, nrow = 1L, ncol = n)
  colnames(mx) <- struct_def$field

  df <- data.frame(mx)
  return(df)
}
