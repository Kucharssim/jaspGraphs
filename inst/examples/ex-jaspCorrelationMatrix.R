# generate correlated data
set.seed(1)
x <- matrix(rnorm(3*1000), ncol = 3)
ch <- structure(c(1, 0, 0, 1.5, 5, 0, 5, -8, 10), dim = c(3L, 3L))
df <- as.data.frame(x %*% ch)
colnames(df) <- c("apples", "oranges", "wine")

# default plot
jaspCorrelationMatrix(data = df)


# custom plotting function with a complicated results object
# (example of jaspRegression/correlation)
pairs <- combn(colnames(df), 2, simplify = FALSE)
pairNames <- sapply(pairs, paste0, collapse = "", simplify = TRUE)

results <- lapply(pairs, function(variables) {
  cor.test(df[[variables[1]]], df[[variables[2]]])
})
names(results) <- pairNames

plotCorStats <- function(x, y, xName, yName, results, options) {
  pairName <- paste0(xName, yName)
  res <- results[[pairName]]

  est <- paste0("r = ", signif(res$estimate, 3))
  ci  <- sprintf("%i%% CI = [%.3f,%.3f]", 100*options[["ciLevel"]], res$conf.int[1], res$conf.int[2])
  plot <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::scale_x_continuous(limits = 0:1) +
    ggplot2::scale_y_continuous(limits = 0:1) +
    ggplot2::geom_text(
      data = data.frame(x = 0.5, y = c(0.7, 0.3), label = c(est, ci)),
      mapping = ggplot2::aes(x = x, y = y, label = label),
      parse = FALSE,
      size = jaspGraphs::getGraphOption("fontsize") / ggplot2::.pt
    )

  return(plot)
}

jaspCorrelationMatrix(
  data           = df,
  bottomLeft     = plotCorStats,
  bottomLeftArgs = list(results = results, options = list(ciLevel = 0.95)),
  topRightArgs   = list(smooth = "lm", smoothCi = TRUE, predict = "ellipse")
)


# group example (jags)
chains <- 3
iter   <- 500
chain <- gl(chains, iter)
df <- data.frame(
  mu    = rnorm(chains*iter, as.integer(chain)),
  sigma = rgamma(chains*iter, shape = as.integer(chain), rate = 1/as.integer(chain))
)

jaspCorrelationMatrix(
  data = df,
  diagonalArgs = list(
    groupingVariable     = chain,
    groupingVariableName = "chain",
    histogramPosition    = "identity",
    density              = FALSE,
    rugs                 = TRUE
  ),
  topRightArgs = list(type = "hex")
)


# or display a trace plot on the diagonal
tracePlot <- function(x, xName, iter, chain) {
  ggplot2::ggplot(data = data.frame(x = x, chain = chain, iter = iter)) +
    ggplot2::geom_line(ggplot2::aes(x = iter, y = x, color = chain, group = chain)) +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe() +
    ggplot2::xlab("Iteration")
}

jaspCorrelationMatrix(
  data                  = df,
  diagonal              = tracePlot,
  diagonalArgs          = list(
    chain = chain,
    iter  = rep(seq_len(iter), times = chains)
  ),
  topRightArgs          = list(type = "hex", bins = 10, palette = "viridis"),
  bottomLeft            = jaspScatter,
  bottomLeftArgs        = list(type = "hex", bins = 10, palette = "viridis"),
  overwriteDiagonalAxes = "y"
)
