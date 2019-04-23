requireNamespace("stats")


#' @importFrom grDevices rgb
#' @rdname radarBoxplot
#' @export
"radarBoxplot.formula" = function(x, data, ...) {
  ### formula interface for radarBoxplot
  ### code gratefully stolen from randomForest.formula (package randomForest).
  ###
  if (!inherits(x, "formula"))
    stop("method is only for formula objects")
  m <- match.call(expand.dots = FALSE)
  ## Catch xtest and ytest in arguments.
  if (any(c("xtest", "ytest") %in% names(m)))
    stop("xtest/ytest not supported through the formula interface")
  names(m)[2] <- "formula"
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m$... <- NULL
  m[[1]] <- as.name("model.frame")
  m <- eval(m, parent.frame())

  y <- model.response(m)
  Terms <- attr(m, "terms")
  attr(Terms, "intercept") <- 0
  ## Drop any "negative" terms in the formula.
  m <- model.frame(terms(reformulate(attributes(Terms)$term.labels)),
                   data.frame(m))
  ## if (!is.null(y)) m <- m[, -1, drop=FALSE]
  for (i in seq(along=m)) {
    if (is.ordered(m[[i]])) m[[i]] <- as.numeric(m[[i]])
  }
  radarBoxplot.default(m, y, ...)
}
