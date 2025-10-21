# Paste the following inside R/retro.R

#' Perform Retrospective Analysis
#'
#' @param stk FLStock object.
#' @param idxs FLIndices object.
#' @param retro Number of retrospective years.
#' @param kfrac Fraction of nodes to remove per step (default = 0.4).
#' @param k Number of nodes.
#' @param ... Additional arguments passed to `sca()`.
#' @return FLStocks object with retrospective fits.
#' @export
retro <- function(stk, idxs, retro = 4, kfrac = 0.4, k, ...) {
  args <- list(...)

  retros_list <- lapply(0:retro, function(x) {
    year_end <- FLCore::range(stk)["maxyear"] - x
    args$stock <- window(stk, end = year_end)
    args$indices <- window(idxs, end = year_end)

    KY <- unname(k - floor(x * kfrac))
    args$fmodel <- update(args$fmodel, list(k = KY))

    fit <- do.call(sca, args)
    args$stock + fit
  })

  FLStocks(retros_list)
}
