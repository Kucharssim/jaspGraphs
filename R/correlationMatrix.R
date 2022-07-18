#' Correlation matrix plot
#'
#' @description Plot that consists of \code{ncol{data}} by \code{ncol{data}} plots,
#' where subplot on position \eqn{(i, j)} plots \code{data[, c(i, j)]}.
#' The plot can display three different types of plots:
#' \describe{
#'   \item{\code{diagonal}}{Where \code{i == j}.}
#'   \item{\code{topRight}}{Where \code{i < j}.}
#'   \item{\code{bottomLeft}}{Where \code{i > j}.}
#' }
#'
#' @param data Data frame of data to plot.
#' @param diagonal A function that draws the plots on the diagonal. Must accept arguments \code{x} (numeric), \code{xName} (character).
#' @param diagonalArgs A list of additional arguments to pass to \code{diagonal}.
#' @param topRight A function that draws the plots on the top right off-diagonal. Must accept arguments \code{x} (numeric), \code{y} (numeric), \code{xName} (character), and \code{yName} (character).
#' @param topRightArgs A list of additional arguments to pass to \code{topRight}.
#' @param bottomLeft A function that draws the plots on the bottom left off-diagonal. Must accept arguments \code{x} (numeric), \code{y} (numeric), \code{xName} (character), and \code{yName} (character).
#' @param bottomLeftArgs A list of additional arguments to pass to \code{bottomLeft}.
#' @param overwriteDiagonalAxes,overwriteTopRightAxes,overwriteBottomLeftAxes Which axes should be overwritten such that they have a common range. Possible options:
#' \describe{
#'   \item{\code{"none"}}{No axes are overwritten, hence the plots get their own scales given by \code{diagonal}, \code{topRight}, and \code{bottomLeft} functions, respectively.}
#'   \item{\code{"both"}}{Both axes are overwritten. The plots inherit scales by setting their \code{breaks} determined by [getPrettyAxisBreaks], and the plotting region is set by [ggplot2::coord_cartesian] with \code{limits} set to \code{range(breaks)}. Further, the name of the axis is set to \code{NULL}.}
#'   \item{\code{"x"}}{x-axis gets overwritten (see option \code{"both"}), y-axis does not (see option \code{"none"}).}
#'   \item{\code{"y"}}{y-axis gets overwritten (see option \code{"both"}), x-axis does not (see option \code{"none"}).}
#' }
#' @param plot Logical; if \code{TRUE}, the plot object is returned, otherwise an object of class \code{jaspGraphsPlot} is returned.
#'
#' @example inst/examples/ex-jaspCorrelationMatrix.R
#' @export
jaspCorrelationMatrix <- function(
    data,
    diagonal       = jaspHistogram,
    diagonalArgs   = list(density = TRUE),
    topRight       = jaspScatter,
    topRightArgs   = list(suppressAxesLabels = TRUE),
    bottomLeft     = NULL,
    bottomLeftArgs = list(),
    overwriteDiagonalAxes   = "x",
    overwriteTopRightAxes   = "both",
    overwriteBottomLeftAxes = "both",
    plot = TRUE
) {

  # validate input
  if (!is.data.frame(data) || nrow(data) == 0 || ncol(data) < 2)
    stop2("`data` must be a data frame")

  data <- data[, vapply(data, is.numeric, logical(1)), drop = FALSE]

  if (ncol(data) < 2)
    stop2("`data` must have more than 2 numeric columns.")

  overwriteDiagonalAxes   <- match.arg(overwriteDiagonalAxes,   choices = c("none", "both", "x", "y"))
  overwriteTopRightAxes   <- match.arg(overwriteDiagonalAxes,   choices = c("none", "both", "x", "y"))
  overwriteBottomLeftAxes <- match.arg(overwriteBottomLeftAxes, choices = c("none", "both", "x", "y"))

  variables <- colnames(data)
  titles    <- c(list(patchwork::plot_spacer()), lapply(variables, .makeTitle))
  # plots <- matrix(data = list(), nrow = ncol(data), ncol = ncol(data))
  plots <- titles
  i <- length(plots) + 1
  for (row in seq_along(variables)) {
    y       <- data[[row]]
    yName   <- variables[[row]]
    yBreaks <- getPrettyAxisBreaks(y)

    plots[[i]] <- .makeTitle(yName, angle = 90)
    i <- i + 1

    for (col in seq_along(variables)) {
      x       <- data[[col]]
      xName   <- variables[[col]]
      xBreaks <- getPrettyAxisBreaks(x)

      if (row == col) { # diagonal
        if(is.function(diagonal)) {
          diagonalArgs[["x"]]   <- x
          diagonalArgs[["xName"]] <- xName
          plot <- .trySubPlot(diagonal, diagonalArgs, overwriteDiagonalAxes, xBreaks, yBreaks)
        } else {
          plot <- ggplot2::ggplot() + jaspGraphs::themeJaspRaw()
        }
      } else if(row < col) { # topRight
        if(is.function(topRight)) {
          topRightArgs[["x"]] <- x
          topRightArgs[["y"]] <- y
          topRightArgs[["xName"]] <- xName
          topRightArgs[["yName"]] <- yName
          plot <- .trySubPlot(topRight, topRightArgs, overwriteTopRightAxes, xBreaks, yBreaks)
        } else {
          plot <- ggplot2::ggplot() + jaspGraphs::themeJaspRaw()
        }
      } else { # bottomLeft
        if(is.function(bottomLeft)) {
          bottomLeftArgs[["x"]] <- x
          bottomLeftArgs[["y"]] <- y
          bottomLeftArgs[["xName"]] <- xName
          bottomLeftArgs[["yName"]] <- yName
          plot <- .trySubPlot(bottomLeft, bottomLeftArgs, overwriteBottomLeftAxes, xBreaks, yBreaks)
        } else {
          plot <- ggplot2::ggplot() + jaspGraphs::themeJaspRaw()
        }
      }
      # plots[[col, row]] <- plot
      plots[[i]] <- plot
      i <- i + 1
    }
  }

  margins <- c(0.05*length(variables), rep(0.95, length(variables)))
  if(plot) {
    out <- patchwork::wrap_plots(plots, ncol = ncol(data)+1, nrow = ncol(data)+1, byrow = TRUE, widths = margins, heights = margins)
  } else {
    out <- jaspGraphsPlot$new(
      subplots     = plots,
      plotFunction = .patchPlots,
      ncol         = ncol(data)+1,
      nrow         = ncol(data)+1,
      widths       = margins,
      heights      = margins,
      byrow        = TRUE
    )
  }
  return(out)
}

.makeTitle <- function(nm, angle = 0) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 1/2, y = 1/2, label = nm, angle = angle,
      size = 1.5 * jaspGraphs::getGraphOption("fontsize") / ggplot2::.pt
    ) +
    ggplot2::ylim(0:1) + ggplot2::xlim(0:1) +
    ggplot2::theme_void()
}


.trySubPlot <- function(fun, args, overwriteAxes, xBreaks, yBreaks) {
  res <- try(do.call(fun, args), silent = TRUE)
  if(inherits(res, "try-error")) {
    message <- as.character(res)
    message <- strsplit(message, ": ")[[1]]
    message <- paste(message[-1], collapse = "")
    message <- strwrap(message, width = 20, initial = gettext("Plotting not possible:\n"))
    message <- paste(message, collapse = "\n")

    res <- ggplot2::ggplot() +
      ggplot2::geom_label(
        data    = data.frame(x = 0.5, y = 0.5, label = message),
        mapping = ggplot2::aes(x = x, y = y, label = label),
        fill    = adjustcolor("red", alpha = 0.5),
        size    = 0.7 * jaspGraphs::getGraphOption("fontsize") / ggplot2::.pt,
        hjust   = "center",
        vjust   = "center"
      ) +
      ggplot2::xlim(0:1) +
      ggplot2::ylim(0:1) +
      ggplot2::theme_void()
  } else {
    if(overwriteAxes %in% c("both", "x")) {
      xLim   <- range(xBreaks)
      xScale <- ggplot2::scale_x_continuous(name = NULL, breaks = xBreaks)
    } else {
      xLim   <- NULL
      xScale <- NULL
    }

    if(overwriteAxes %in% c("both", "y")) {
      yLim   <- range(yBreaks)
      yScale <- ggplot2::scale_y_continuous(name = NULL, breaks = yBreaks)
    } else {
      yLim   <- NULL
      yScale <- NULL
    }

    res <- res + xScale + yScale + ggplot2::coord_cartesian(xlim = xLim, ylim = yLim)
  }

  return(res)
}

.patchPlots <- function(subplots, args, decodeplotFun = get0("decodeplot"), ...) {

  if(!is.null(decodeplotFun))
    g <- lapply(subplots, decodeplotFun, returnGrob = FALSE)

  g <- patchwork::wrap_plots(
    subplots,
    ncol    = args[["ncol"]],
    nrow    = args[["nrow"]],
    widths  = args[["widths"]],
    heights = args[["heights"]],
    byrow   = args[["byrow"]]
  )

  return(g)
}

#' Scatter plot
#'
#' @description This plot consists of three layers:
#' \enumerate{
#'   \item The distribution of the data displayed as [geom_point] or as a [ggplot2::geom_hex].
#'   \item Smooth line through the data displayed using [ggplot2::geom_smooth].
#'   \item Prediction interval of y given x using [stats::predict.lm](assuming linear relationship), or prediction ellipse assuming bivariate normal distribution.
#' }
#' @param x Numeric vector of values on the x-axis.
#' @param y Numeric vector of values on the y-axis.
#' @param xName Character; x-axis label.
#' @param yName Character; y-axis label.
#' @param type Character; How should the distribution of the data be displayed:
#' \describe{
#'    \item{"point"}{Using [geom_point].}
#'    \item{"hex"}{Using [ggplot2::geom_hex]}
#'    \item{"auto"}{If the number of data points is smaller than 1000, using [geom_point], otherwise using [ggplot2::geom_hex].}
#' }
#' @param bins,binwidth Arguments passed to [ggplot2::geom_hex].
#' @param palette Argument passed to [JASPcolors]. Palette to use for drawing [ggplot2::geom_hex].
#' @param fill Argument passed to [geom_point].
#' @param alpha Argument passed to [geom_point] or [ggplot2::geom_hex].
#' @param smooth Character; passed as \code{method} argument to [ggplot2::geom_smooth],
#' unless \code{smooth == "none"}, in which case the layer si not plotted.
#' @param smoothCi Logical; Should confidence interval around the smooth line be plotted?
#' Passed as \code{se} argument to [ggplot2::geom_smooth].
#' @param smoothCiLevel Numeric; Confidence level of the confidence interval around the smooth line.
#' Passed as \code{level} argument to [ggplot2::geom_smooth].
#' @param smoothColor Color of the smooth line.
#' @param predict Character; Method for drawing the prediction interval:
#' \describe{
#'   \item{"none"}{Prediction interval is not displayed.}
#'   \item{"lm"}{Prediction interval is plotted, the confidence bands are calculated using [stats::predict.lm].}
#'   \item{"ellipse"}{Prediction ellipse is plotted, assuming bi-variate normal model.}
#' }
#' @param predictCiLevel Numeric; Confidence level of the prediction interval.
#' @param predictColor Color of the prediction interval.
#' @param suppressAxesLabels Logical; should axis labels be suppressed.
#' @export
jaspScatter <- function(
  x, y, xName = NULL, yName = NULL,
  type = c("point", "hex", "auto"),
  bins = 30, binwidth = NULL, palette = "gray", fill = "gray", alpha = 1,
  smooth = c("none", "lm", "glm", "gam", "loess"),
  smoothCi = FALSE, smoothCiLevel = 0.95,
  smoothColor = adjustcolor("darkred", alpha = 0.5),
  predict = c("none", "lm", "ellipse"),
  predictCiLevel = 0.95,
  predictColor = adjustcolor("steelblue", alpha = 0.5),
  suppressAxesLabels = FALSE
) {

  df <- data.frame(x = x, y = y)

  type <- match.arg(type)
  if(type == "auto" && nrow(df) >= 1000) type <- "hex" else "point"

  if(type == "point") {
    base_layer <- jaspGraphs::geom_point(fill = fill, alpha = alpha)
  } else {
    base_layer <- ggplot2::geom_hex(mapping = ggplot2::aes(fill = ..ncount..), alpha = alpha, bins = bins, binwidth = binwidth)
  }

  smooth <- match.arg(smooth)
  if(smooth != "none") {
    formula <- switch (smooth,
      gam = y ~ s(x, bs = "cs"),
      y ~ x
    )
    smooth_layer <- ggplot2::geom_smooth(
      method  = smooth,
      se      = smoothCi,
      level   = smoothCiLevel,
      color   = smoothColor,
      fill    = smoothColor,
      formula = formula
    )
  } else {
    smooth_layer <- NULL
  }

  predict <- match.arg(predict)
  if(predict == "lm") {
    fit <- lm(y~x, data = df)
    preds <- predict(fit, newdata = df, interval = "prediction", level = predictCiLevel)
    preds <- as.data.frame(preds)
    preds[["x"]] <- df[["x"]]
    predict_layer <- ggplot2::geom_ribbon(
      data    = preds,
      mapping = ggplot2::aes(x = x, ymin = lwr, ymax = upr),
      fill    = predictColor
    )
  } else if(predict == "ellipse") {
    predict_layer <- ggplot2::stat_ellipse(
      geom  = "polygon",
      fill  = predictColor,
      type  = "t",
      level = predictCiLevel
    )
  } else {
    predict_layer <- NULL
  }

  if(suppressAxesLabels)
    xName <- yName <- NULL

  plot <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y)) +
    predict_layer +
    smooth_layer +
    base_layer +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe() +
    ggplot2::xlab(xName) +
    ggplot2::ylab(yName) +
    ggplot2::scale_fill_gradientn(limits = 0:1, colors = JASPcolors(palette = palette))

  return(plot)
}
