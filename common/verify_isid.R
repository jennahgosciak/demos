# checks uniqueness of dataframe, can be used in a pipe
verify_isid <- function(df, vars) {
  df %>%
    isid(vars) %>%
    stopifnot()

  return(df)
}
