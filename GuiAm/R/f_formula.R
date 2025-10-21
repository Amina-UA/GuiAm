# Paste the following into R/f_formula.R

#' Construct GAM Formula Based on String Code
#'
#' @param model_string A model string (e.g., "sy", "sa", or "sy+sa").
#' @param ks List of node values (must match the number of model terms).
#' @return A formula object.
#' @export
f <- function(model_string, ks) {
  model_map <- list(
    "sy"  = function(k) substitute(s(year, k = K), list(K = k)),
    "sa"  = function(k) substitute(s(age,  k = K), list(K = k)),
    "syt" = function(k1, k2) substitute(te(age, year, k = c(K1, K2)),
                                        list(K1 = k1, K2 = k2))
  )

  terms <- strsplit(model_string, "\\+")[[1]]
  terms <- trimws(tolower(terms))

  if (length(terms) != length(ks)) {
    stop("Number of terms and number of k-values do not match.")
  }

  formula_terms <- mapply(function(term, k) {
    fn <- model_map[[term]]
    if (is.null(fn)) stop(paste("Invalid model term:", term))
    if (length(k) > 1) do.call(fn, as.list(k)) else fn(k)
  }, terms, ks, SIMPLIFY = FALSE)

  Reduce(function(a, b) call("+", a, b), formula_terms) |>
    (function(expr) as.formula(call("~", expr)))()
}
