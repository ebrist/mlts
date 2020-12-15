
#' Importance Plot for XGBoost
#'
#' Plot variable importance from an xgboost model. 
#'
#' @param xgb_importance Variable importance from an xgboost model. See \link[xgboost]{xgb.importance}.
#' @param top_n Number of variables to plot. Default is top_n = 25. 
#'
#' @return A ggplot.
#' @export
#'
importance_plot <- function(xgb_importance, top_n = 25) {
  xgb_importance <- dplyr::slice(xgb_importance, 1:top_n)
  ggplot2::ggplot(xgb_importance, ggplot2::aes(x = Feature, y = Gain)) +
    ggplot2::theme_bw() + 
    ggplot2::labs(title = "XGBoost Predictor Importance") +
    ggplot2::coord_flip() + 
    ggplot2::geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) +
    ggplot2::scale_x_discrete(limits = rev(xgb_importance$Feature))
}