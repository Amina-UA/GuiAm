#' Generate Model Fit Summary (AIC, BIC, GCV)
#'
#' @param fits A list of fits from loop_scas().
#' @param ks Optional vector of model complexity values.
#' @return A data.frame with k, AIC, BIC, GCV.
#' @export
model_summary <- function(fits, ks = NULL) {
  if (is.null(ks)) ks <- seq_along(fits)

  data.frame(
    k = ks,
    AIC = sapply(fits, AIC),
    BIC = sapply(fits, BIC),
    GCV = sapply(fits, function(x) x@fitSumm["gcv", ])
  )
}