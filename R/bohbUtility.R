#' @keywords internal
EI <- function(..., lkde, gkde) { predict(lkde, x = c(...)) / predict(gkde, x = c(...)) }

#' @keywords internal
map_all <- function(df) {
  do.call("mapply", c(list, df, SIMPLIFY = FALSE, USE.NAMES=FALSE))
}

#' @keywords internal
coalesce_all_columns <- function(df, group_vars = NULL) {

  if (is.null(group_vars)) {
    group_vars <-
      df %>%
      purrr::keep(~ dplyr::n_distinct(.x) == 1L) %>%
      names()
  }

  msk <- colnames(df) %in% group_vars
  same_df <- df[1L, msk, drop = FALSE]
  coal_df <- df[, !msk, drop = FALSE] %>%
    purrr::map_dfc(na.omit)

  cbind(same_df, coal_df)
}
