#' Forecast Plot 
#' 
#' Plot forecasts from \link[MLTS]{dhr_cv} or \link[MLTS]{mlts_cv} output. 
#'
#' @param mlts_mod An output list from \link[MLTS]{dhr_cv} or \link[MLTS]{mlts_cv}.
#' @param start Starting time for the plot. By default, the entire series is plotted (start = 1). 
#' @param shade Option to shade the forecast portion of the plot. 
#' @param colors Option to set the line colors. 
#' @param x_label Option to set the x-axis label.
#' @param y_label Option to set the y-axis label.
#' @param color_labels Option to set the color labels. 
#' @param title Option to set the title.
#' @param lsize Option to set the line size. 
#' @param interactive Option for an interactive plot. 
#'
#' @return a ggplotly or plotly 
#' @export
fplot <- function(mlts_mod, start = 1, shade = T, colors = c("#347B98", "#FB9902"),
                  x_label = "Time", y_label = "Series",
                  color_labels = c("Observed", "Predicted"), 
                  title = "Forecast Plot", lsize = NULL, interactive = T) {
  # na fill length
  na_length <- length(mlts_mod$y) - length(mlts_mod$y_fitted) - length(mlts_mod$y_hat)
  # plot data
  dat <- data.frame(obs = as.numeric(mlts_mod$y), 
                    y_hat = c(rep(NA, na_length), mlts_mod$y_fitted, mlts_mod$y_hat), 
                    time = 1:length(as.numeric(mlts_mod$y)))
  if (start > 1) {
    dat <- dat[-c(1:(start - 1)), ]
  }
  # y forecast limits
  if (interactive) {
    ymax <- max(c(dat$obs, dat$y_hat), na.rm = T) 
    ymin <- min(c(dat$obs, dat$y_hat), na.rm = T)
  } else {
    ymax = Inf
    ymin = -Inf
  }
  # x forecast limits
  xmin <- length(mlts_mod$y) - length(mlts_mod$y_hat)
  xmax <- length(mlts_mod$y)
  # reorder colors
  color_order <- order(color_labels)
  colors <- colors[color_order]
  # set line size
  if (is.null(lsize)) {
    lsize <- ifelse(interactive, 0.5, 1)
  }
  # ggplot
  suppressMessages(
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = time)) +
      ggplot2::geom_line(ggplot2::aes(y = obs, color = color_labels[1]), 
                        size = lsize, na.rm = T) +
      ggplot2::geom_line(ggplot2::aes(y = y_hat, color = color_labels[2]), 
                        size = lsize, na.rm = T) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = x_label, y = y_label, title = title) + 
      ggplot2::scale_color_discrete(name = NULL)  +
      ggplot2::scale_color_manual("", breaks = color_labels, values = colors) 
  )
  suppressMessages(
    if (min(dat$time) < xmin & shade) {
      p <- p + 
        ggplot2::geom_segment(x = xmin, xend = xmin, y = ymin, 
                              yend = ymax, size = 0.5, linetype = "dashed") +
        ggplot2::annotate("rect", xmin = xmin, xmax = xmax, 
                              ymin = ymin, ymax = ymax, alpha = 0.2)
    }
  )
  # return ggplot if not interactive
  if (!interactive) {
    return(p)
  }
  # return ggplotly if interactive 
  g <- plotly::layout(plotly::add_annotations(plotly::ggplotly(p),
                                              yref = "paper", xref = "paper", y = 1.1, x = 0, 
                                              text = title, showarrow = F, font = list(size = 17)),
                      title = FALSE)
  # return ggplotly
  g
}

