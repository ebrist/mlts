#' Print Forecast Simulation Results
#'
#' @param sim An output list from a simulation produced by \link[MLTS]{sim}.
#' @param scipen Option to set scipen. 
#' @param digits Option to set number of digits for rounding. 
#' @param caption Option to add table caption. 
#'
#' @return A tibble of simulation results.
#' @export
#' 
print_sim <- function(sim, scipen = 999, digits = 4, caption = "Simulation Results") {
  options(scipen = scipen)
  pander::panderOptions("digits", digits)
  t <- tibble::tibble(Method = c("ARIMA", "DHR", "XGBoost", "RF", "SVM"),
                      `Runtime (mins)` = c(sim$arima_rt, sim$dhr_rt, 
                                           sim$xgb_rt, sim$rf_rt, sim$svm_rt),
                      RMSE = c(sim$arima_rmse, sim$dhr_rmse, 
                               sim$xgb_rmse, sim$rf_rmse, sim$svm_rmse))
  pander::pander(t, caption = caption)
}
