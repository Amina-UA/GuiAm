# Paste into R/loop_scas.R

#' Loop Over SCA Fits for Different Model Complexities
#'
#' @param stk FLStock object.
#' @param idx FLIndex object.
#' @param expression Model expression string ("sy", "sa", or "sy+sa").
#' @param qmodel Q model object.
#' @param srmodel SR model object.
#' @return List of fits from scas().
#' @export
loop_scas <- function(stk, idx, expression = 'sy', qmodel, srmodel) {
  time_range <- stk@range["minyear"]:stk@range["maxyear"]
  age_range <- stk@range["min"]:stk@range["max"]

  len_ts <- length(time_range)
  len_age <- length(age_range)

  expression <- tolower(gsub(" ", "", expression))

  if (expression == 'sy') {
    ks <- (floor(len_ts / 2) - 3):(floor(len_ts / 2) + 5)
    fmodels <- lapply(ks, function(k) f("sy", list(k)))
  } else if (expression == 'sa') {
    ks <- (floor(len_age / 2) - 3):(floor(len_age / 2) + 5)
    fmodels <- lapply(ks, function(k) f("sa", list(k)))
  } else if (expression %in% c('sy+sa', 'sa+sy')) {
    param_grid <- expand.grid(
      i = (floor(len_ts / 2) - 3):(floor(len_ts / 2) + 5),
      j = (len_age - 2):(len_age - 1)
    )
    fmodels <- apply(param_grid, 1, function(row) f("sy+sa", as.list(row)))
  } else {
    stop("Unsupported model expression.")
  }

  fits <- FLa4a::scas(
    FLCore::FLStocks(stk),
    list(FLCore::FLIndices(idx)),
    fmodels,
    list(qmodel),
    list(srmodel)
  )

  return(fits)
}
