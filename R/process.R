
#' Pre-process a dataframe for modeling with AIPW or TMLE
#'
#' @param df a dataframe or tibble
#'
#' @return a dataframe with missing values imputed at the median, an indicator for missingness, and factors dummy coded.
#' @export
process <- function(df) {
  as.data.frame(model.matrix( ~ ., data = as.data.frame(
    sl3::make_sl3_Task(data = df, covariates = names(df))$data
  ))[, -1])
}
