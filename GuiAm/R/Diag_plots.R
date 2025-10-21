#' Diagnostic Plots for Model Fits
#'
#' @param fits A list of model fits (e.g., from loop_scas or custom fmodels).
#' @param stk FLStock object (used in residuals and fit plots).
#' @param idx FLIndex object (used in residuals and fit plots).
#' @param outdir Folder where plots will be saved (default: "plots/diagnostics").
#' @param prefix Optional filename prefix.
#' @export
Diag_plots <- function(fits, stk, idx, outdir = "plots_diagnostics", prefix = "fit") {
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

  for (j in seq_along(fits)) {
    fit <- fits[[j]]
    res <- residuals(fit, stk, idx)

    # Define file helper
    out_file <- function(name) file.path(outdir, paste0(prefix, "_", name, "_", j, ".jpg"))

    # Residual plot
    jpeg(out_file("residuals"), width = 2000, height = 1500, res = 300)
    print(plot(res))
    dev.off()

    # Bubble plot
    jpeg(out_file("bubbles"), width = 2000, height = 1500, res = 300)
    print(bubbles(res))
    dev.off()

    # QQ plot
    jpeg(out_file("qqmath"), width = 2000, height = 1500, res = 300)
    print(qqmath(res))
    dev.off()

    # Fit vs stock
    jpeg(out_file("fit_stk"), width = 2000, height = 1500, res = 300)
    print(plot(fit, stk))
    dev.off()

    # Fit vs index
    jpeg(out_file("fit_idx"), width = 2000, height = 1500, res = 300)
    print(plot(fit, idx))
    dev.off()
  }
}
