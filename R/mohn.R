# Paste the following into R/mohn.R

#' Compute Mohn's Rho
#'
#' @param retro.lst A list of FLStock objects (output from `retro()`).
#' @param qoi Quantities of interest: one or more of "fbar", "ssb", "rec".
#' @return Named numeric vector of Mohn's rho values.
#' @export
mohn <- function(retro.lst, qoi = c("fbar", "ssb", "rec")) {
  qoi <- match.arg(qoi, several.ok = TRUE)
  rho_values <- setNames(numeric(length(qoi)), qoi)

  yrs <- sapply(retro.lst, function(x) range(x)["maxyear"])
  base_years <- yrs[-1]

  for (q in qoi) {
    series_list <- lapply(retro.lst, q)
    base <- series_list[[1]][, as.character(base_years)]
    retros <- sapply(series_list[-1], function(x) x[, dim(x)[2]])
    rho_values[q] <- mean(retros / base - 1, na.rm = TRUE)
  }

  return(rho_values)
}
