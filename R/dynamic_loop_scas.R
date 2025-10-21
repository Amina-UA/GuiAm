#' Generate fits from dynamic fmodels based on a k-range
#'
#' @param stk FLStock
#' @param idx FLIndex
#' @param k_range A vector of k values (default = 1:20)
#' @param qmodel qmodel
#' @param srmodel srmodel
#' @return List of fits
#' @export
dynamic_loop_scas <- function(stk, idx, k_range = 1:20, qmodel, srmodel) {
  fmods <- lapply(k_range, function(k) {
    as.formula(paste0("~factor(age) + s(year, k=", k, ")"))
  })

  names(fmods) <- paste0("fmodel", k_range)

  fits <- scas(
    FLStocks(stk),
    list(FLIndices(idx)),
    fmods,
    list(qmodel),
    list(srmodel)
  )

  return(fits)
}
