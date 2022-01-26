######################################################
# produce merge with statistics
# mirrors behavior of merge in stata
merge_qc <- function(df) {
  df %>%
    mutate(merge = case_when(merge.x == 1 & merge.y == 1 ~ 3,
                             merge.x == 1 & is.na(merge.y) ~ 1,
                             is.na(merge.x) & merge.y == 1 ~ 2)) %>%
    select(-c("merge.x", "merge.y"))
}

# option verify, checks that merge is inline with the values supplied
# e.g. verify = C(2,3) checks that the merge is only right data and merge data
# 1 = left file only, 2 = right file only, 3 = merged
# tab prints merge statistics
full_join_qc <- function(x, y, by = NULL, tab = F, verify = NULL, drop_ind = F, ...) {
  df <- full_join(mutate(x, merge = 1), mutate(y, merge = 1), by = by, ...) %>%
    merge_qc()

  if (tab == T) {
    df %>%
      tablist_qc(merge)
  }

  if (!is.null(verify)) {
    if (all(is.character(verify))) {
      warning('verify must be numeric')
    } else if (all(verify %notin% c(1, 2, 3))) {
      warning('verify uses stata syntax: 1 = left, 2 = right, 3 = merged')
    }

    stopifnot(setequal(unique(df$merge), verify))
  }

  if (drop_ind == T) {
    df <- df %>%
      select(-"merge")
  }
  return(df)
}

inner_join_qc <- function(x, y, by = NULL, tab = F, verify = NULL, drop_ind = F, ...) {

  df <- full_join_qc(x, y, by, tab = tab, verify = verify, ...) %>%
    filter(merge == 3)

  return(df)
}

left_join_qc <- function(x, y, by = NULL, tab = F, verify = NULL, drop_ind = F, ...) {

  df <- full_join_qc(x, y, by, tab = tab, verify = verify, ...) %>%
    filter(merge %in% c(1, 3))
  return(df)
}

right_join_qc <- function(x, y, by = NULL, tab = F, verify = NULL, drop_ind = F, ...) {

  df <- full_join_qc(x, y, by, tab = tab, verify = verify, ...) %>%
    filter(merge %in% c(2, 3))
  return(df)
}
