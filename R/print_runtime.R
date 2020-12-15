#' Print Simulation Runtimes
#'
#' @param out An output list from a simulation produced by \link[MLTS]{sim}.
#'
#' @return A tibble of runtimes. 
#' @export
print_runtime <- function(out) {
  rt <- tibble(
    Simulation = paste0("Sim ", 1:length(out$n_sim)),
    `Train Length` = out$n_sim - ceiling(out$n_test),
    `ARIMA RT` = out$runtime_arima,
    `DHR RT` = out$runtime_dhr,
    `XGB RT` = out$runtime_xgb,
    `RF RT` = out$runtime_rf,
    `SVM RT` = out$runtime_svm)
  rt
}
